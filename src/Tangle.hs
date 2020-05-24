-- ~\~ language=Haskell filename=src/Tangle.hs
-- ~\~ begin <<lit/13-tangle.md|src/Tangle.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Tangle where

import RIO hiding (try, some, many)
import qualified RIO.Text as T
import qualified RIO.Map as M
-- ~\~ begin <<lit/01-entangled.md|import-lazy-map>>[0]
import qualified Data.Map.Lazy as LM
-- ~\~ end

-- ~\~ begin <<lit/01-entangled.md|import-megaparsec>>[0]
import Text.Megaparsec
    ( MonadParsec, Parsec, parse
    , chunk, many, some, eof
    , manyTill, anySingle, try, lookAhead, takeWhile1P, takeWhileP
    , (<?>), getParserState, pstateSourcePos, statePosState, sourceLine, unPos )
import Text.Megaparsec.Char
    ( space )
-- ~\~ end

import Control.Monad.State (MonadState, gets, modify, StateT, evalStateT)

import ListStream
import Document
import Config (config, HasConfig, Config, lookupLanguage, ConfigLanguage(..) )

-- ~\~ begin <<lit/13-tangle.md|tangle-imports>>[0]
import Attributes (attributes)
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|tangle-imports>>[1]
-- import ListStream (parseLine, parseLineNot, tokenLine)
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|tangle-imports>>[2]
import TextUtil (indent, unlines')
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|tangle-imports>>[3]
import Comment (annotateComment)
-- ~\~ end

-- ~\~ begin <<lit/13-tangle.md|parse-markdown>>[0]
codeHeader :: (MonadParsec e Text m)
           => m [CodeProperty]
codeHeader = do
    chunk "```" >> space >> chunk "{" >> space
    props <- attributes
    chunk "}" >> space >> eof
    return props

codeFooter :: (MonadParsec e Text m)
           => m ()
codeFooter = chunk "```" >> space >> eof
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-markdown>>[1]
getLanguage :: ( MonadReader Config m )
            => [CodeProperty] -> m ProgrammingLanguage
getLanguage [] = return NoLanguage
getLanguage (CodeClass cls : _)
    = maybe (UnknownClass cls)
            KnownLanguage
            <$> asks (\cfg -> languageName <$> lookupLanguage cfg cls)
getLanguage (_ : xs) = getLanguage xs
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-markdown>>[2]
getReference :: ( MonadState ReferenceCount m )
             => [CodeProperty] -> m (Maybe ReferenceId)
getReference [] = return Nothing
getReference (CodeId x:_) = Just <$> newReference (ReferenceName x)
getReference (CodeAttribute k v:xs)
    | k == "file" = do
        a <- getReference xs
        b <- Just <$> newReference (ReferenceName v)
        return $ a <|> b
    | otherwise   = getReference xs
getReference (_:xs) = getReference xs
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-markdown>>[3]
getFilePath :: [CodeProperty] -> Maybe FilePath
getFilePath [] = Nothing
getFilePath (CodeAttribute k v:xs)
    | k == "file" = Just $ T.unpack v
    | otherwise   = getFilePath xs
getFilePath (_:xs) = getFilePath xs

getFileMap :: [ReferencePair] -> FileMap
getFileMap = M.fromList . mapMaybe filePair
    where filePair (ref, CodeBlock{..}) = do
              path <- getFilePath codeProperties
              case codeLanguage of
                  KnownLanguage l -> return (path, (referenceName ref, l))
                  _               -> Nothing
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-markdown>>[4]
data ReferenceCount = ReferenceCount
    { currentDocument :: FilePath
    , refCounts       :: Map ReferenceName Int }

countReference :: ( MonadState ReferenceCount m )
               => ReferenceName -> m Int
countReference r = do
    x <- gets (M.findWithDefault 0 r . refCounts)
    modify $ \s -> s { refCounts = M.insert r (x + 1) (refCounts s) }
    return x

newReference :: ( MonadState ReferenceCount m )
             => ReferenceName -> m ReferenceId
newReference n = do
    doc <- gets currentDocument
    x   <- countReference n
    return $ ReferenceId doc n x
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-markdown>>[5]
type DocumentParser = ReaderT Config (StateT ReferenceCount (Parsec Void (ListStream Text)))

codeBlock :: ( MonadParsec e (ListStream Text) m
             , MonadReader Config m
             , MonadState ReferenceCount m )
          => m ([Content], [ReferencePair])
codeBlock = do
    (props, begin) <- tokenLine (parseLine codeHeader)
    code           <- unlines'
                   <$> manyTill (anySingle <?> "code line")
                                (try $ lookAhead $ tokenLine (parseLine codeFooter))
    (_, end)       <- tokenLine (parseLine codeFooter)
    language       <- getLanguage props
    linenum        <- Just . unPos . sourceLine . pstateSourcePos . statePosState <$> getParserState
    ref'           <- getReference props
    return $ case ref' of
        Nothing  -> ( [ PlainText $ unlines' [begin, code, end] ], [] )
        Just ref -> ( [ PlainText begin, Reference ref, PlainText end ]
                    , [ ( ref, CodeBlock language props code linenum ) ] )

normalText :: ( MonadParsec e (ListStream Text) m )
           => m ([Content], [ReferencePair])
normalText = do
    text <- unlines' <$> some (tokenLine (parseLineNot codeHeader))
    return ( [ PlainText text ], [] )

markdown :: DocumentParser ([Content], [ReferencePair])
markdown = mconcat <$> many (codeBlock <|> normalText)

parseMarkdown' :: ( MonadReader env m, HasConfig env, MonadThrow m )
               => FilePath -> Text -> m Document
parseMarkdown' f t = do
    cfg <- view config
    let result' = parse (evalStateT (runReaderT markdown cfg) (ReferenceCount f mempty))
                        f (ListStream $ T.lines t)
    case result' of
        Left err              -> throwM $ TangleError $ tshow err
        Right (content, refs) -> return $ Document (M.fromList refs) content (getFileMap refs)
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-code>>[0]
data CodeLine = PlainCode Text
              | NowebReference ReferenceName Text
              deriving (Eq, Show)

type CodeParser = Parsec Void Text
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-code>>[1]
nowebReference :: CodeParser CodeLine
nowebReference = do
    indent' <- takeWhileP Nothing (`elem` (" \t" :: String))
    _ <- chunk "<<"
    id' <- takeWhile1P Nothing (`notElem` (" \t<>" :: String))
    _ <- chunk ">>"
    space >> eof
    return $ NowebReference (ReferenceName id') indent'

parseCode :: ReferenceName -> Text -> [CodeLine]
parseCode name = map parseLine' . T.lines
    where parseLine' l = either (const $ PlainCode l) id
                       $ parse nowebReference
                              (T.unpack $ unReferenceName name)
                              l
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-code>>[2]
type ExpandedCode = LM.Map ReferenceName (Either EntangledError Text)
type Annotator = ReferenceMap -> ReferenceId -> Either EntangledError Text
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-code>>[3]
expandedCode :: Annotator -> ReferenceMap -> ExpandedCode
expandedCode annotate refs = result
    where result = LM.fromSet expand (referenceNames refs)
          expand name = unlines' <$> mapM
                        (annotate refs >=> expandCodeSource result name)
                        (referencesByName refs name)

expandCodeSource :: ExpandedCode -> ReferenceName -> Text
                 -> Either EntangledError Text
expandCodeSource result name t
    = unlines' <$> mapM codeLineToText (parseCode name t)
    where codeLineToText (PlainCode x) = Right x
          codeLineToText (NowebReference name' i)
              = indent i <$> result LM.! name'
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-code>>[4]
annotateNaked :: ReferenceMap -> ReferenceId -> Either EntangledError Text
annotateNaked refs ref = maybe (Left $ TangleError $ "reference not found: " <> tshow ref)
                               (Right . codeSource)
                               (refs M.!? ref)

annotateComment' :: Config -> Annotator
annotateComment' cfg rmap rid = runReaderT (annotateComment rmap rid) cfg
-- ~\~ end
-- ~\~ end
