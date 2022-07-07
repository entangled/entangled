-- ~\~ language=Haskell filename=src/Tangle.hs
-- ~\~ begin <<lit/13-tangle.md|src/Tangle.hs>>[init]
{-# LANGUAGE NoImplicitPrelude,ScopedTypeVariables #-}
module Tangle where

import RIO hiding (try, some, many)
import qualified RIO.Text as T
import qualified RIO.Map as M
-- ~\~ begin <<lit/01-entangled.md|import-lazy-map>>[init]
import qualified Data.Map.Lazy as LM
-- ~\~ end

import Text.Megaparsec
    ( MonadParsec, Parsec, parse, getOffset
    , chunk, many, some, eof
    , manyTill, anySingle, try, lookAhead, takeWhile1P, takeWhileP
    , (<?>) )
import Text.Megaparsec.Char
    ( space )

import Control.Monad.State (MonadState, gets, modify, StateT, evalStateT)
import Control.Monad.Except ( MonadError )

import ListStream
import Document
import Config (config, HasConfig, Config(..), lookupLanguage, ConfigLanguage(..), AnnotateMethod(..), ConfigSyntax(..))

-- ~\~ begin <<lit/13-tangle.md|tangle-imports>>[init]
import Text.Regex.TDFA
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|tangle-imports>>[1]
-- import ListStream (parseLine, parseLineNot, tokenLine)
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|tangle-imports>>[2]
import TextUtil (indent, unlines')
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|tangle-imports>>[3]
import Comment (annotateComment, annotateProject, annotateNaked)
-- ~\~ end

-- ~\~ begin <<lit/13-tangle.md|parse-markdown>>[init]
type Match = Maybe (Text, Text, Text, [Text])

matchCodeHeader :: ConfigSyntax -> Text -> Maybe ([CodeProperty], Text)
matchCodeHeader syntax line =
    if line =~ matchCodeStart syntax
    then Just (fromMaybe [] (getLanguage' <> getFileName <> getReferenceName <> getHeaderLen), line)
    else Nothing
    where
          getLanguage' :: Maybe [CodeProperty]
          getLanguage' = do
            (_, _, _, lang) <- line =~~ extractLanguage syntax :: Match
            return (map CodeClass lang)

          getFileName :: Maybe [CodeProperty]
          getFileName = do
            (_, _, _, file) <- line =~~ extractFileName syntax :: Match
            return (map (CodeAttribute "file") file)

          getReferenceName :: Maybe [CodeProperty]
          getReferenceName = do
            (_, _, _, ref)  <- line =~~ extractReferenceName syntax :: Match
            return (map CodeId ref)

          getHeaderLen :: Maybe [CodeProperty]
          getHeaderLen = do
            (_, _, _, headLen) <- line =~~ extractProperty syntax "header" :: Match
            return $ CodeAttribute "header" <$> headLen

matchCodeFooter :: ConfigSyntax -> Text -> Maybe ((), Text)
matchCodeFooter syntax line =
    if line =~ matchCodeEnd syntax
    then Just ((), line)
    else Nothing
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
type DocumentParser = ReaderT Config (StateT ReferenceCount (Parsec Text (ListStream Text)))

codeBlock :: ( MonadParsec e (ListStream Text) m
             , MonadReader Config m
             , MonadState ReferenceCount m )
          => m ([Content], [ReferencePair])
codeBlock = do
    Config{..} <- ask
    -- linenum        <- Just . unPos . sourceLine . pstateSourcePos . statePosState <$> getParserState
    linenum        <- Just . (+ 2) <$> getOffset
    (props, begin) <- tokenLine (matchCodeHeader configSyntax)
    code           <- unlines'
                   <$> manyTill (anySingle <?> "code line")
                                (try $ lookAhead $ tokenLine (matchCodeFooter configSyntax))
    (_, end)       <- tokenLine (matchCodeFooter configSyntax)
    language       <- getLanguage props
    ref'           <- getReference props
    return $ case ref' of
        Nothing  -> ( [ PlainText $ unlines' [begin, code, end] ], [] )
        Just ref -> ( [ PlainText begin, Reference ref, PlainText end ]
                    , [ ( ref, CodeBlock language props code linenum ) ] )

normalText :: ( MonadParsec e (ListStream Text) m
              , MonadReader Config m )
           => m ([Content], [ReferencePair])
normalText = do
    Config{..} <- ask
    text <- unlines' <$> some (tokenLineNot $ matchCodeHeader configSyntax)
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
-- ~\~ begin <<lit/13-tangle.md|generate-code>>[init]
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
    where parseLine' l = fromRight (PlainCode l)
                       $ parse nowebReference
                              (T.unpack $ unReferenceName name)
                              l
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-code>>[2]
type ExpandedCode m = LM.Map ReferenceName (m Text)
type Annotator m = ReferenceMap -> Bool -> ReferenceId -> m Text
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-code>>[3]
expandedCode :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => Annotator m -> ReferenceMap -> ExpandedCode m
expandedCode annotate refs = result
    where result = LM.fromSet expand (referenceNames refs)
          expand name = expandRefs name (referencesByName refs name)
          expandRefs _ [] = return ""
          expandRefs name (a:as) = do
              annotFirst <- annotate refs True a >>= expandCodeSource result name
              annotRest  <- mapM (annotate refs False >=> expandCodeSource result name) as
              return $ unlines' (annotFirst:annotRest)

expandCodeSource :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => ExpandedCode m -> ReferenceName -> Text -> m Text
expandCodeSource result name t
    = unlines' <$> mapM codeLineToText (parseCode name t)
    where codeLineToText (PlainCode x) = return x
          codeLineToText (NowebReference name' i)
              = indent i <$> fromMaybe (logWarn ("unknown reference <<" <> display name' <> ">> in #" <> display name) >> return "")
                                       (result LM.!? name')
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-code>>[4]
selectAnnotator :: (MonadError EntangledError m) => Config -> Annotator m
selectAnnotator cfg@Config{..} = case configAnnotate of
    AnnotateNaked         -> \rmap init rid -> runReaderT (annotateNaked rmap init rid) cfg
    AnnotateStandard      -> \rmap init rid -> runReaderT (annotateComment rmap init rid) cfg
    AnnotateProject       -> \rmap init rid -> runReaderT (annotateProject rmap init rid) cfg
-- ~\~ end
-- ~\~ end
