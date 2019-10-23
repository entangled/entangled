-- ------ language="Haskell" file="src/Tangle.hs"
module Tangle where

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ begin <<import-map>>[0]
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
-- ------ end
-- ------ begin <<import-lazy-map>>[0]
import qualified Data.Map.Lazy as LM
-- ------ end

-- ------ begin <<import-megaparsec>>[0]
import Text.Megaparsec
    ( MonadParsec, Parsec, parse
    , chunk, many, some, eof
    , manyTill, anySingle, try, lookAhead, takeWhile1P, takeWhileP
    , (<|>), (<?>) )
import Text.Megaparsec.Char
    ( space )
import Data.Void
-- ------ end

import Control.Monad.Reader (MonadReader, reader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, gets, modify, StateT, evalStateT)
import Control.Monad ((>=>))

import Data.Maybe (catMaybes)

import ListStream (ListStream (..))
import Document
    ( CodeProperty (..), CodeBlock (..), Content (..), ReferenceId (..)
    , ReferenceName (..), ProgrammingLanguage (..), Document (..), FileMap
    , EntangledError (..), referenceNames, referencesByName )
import Config (Config, lookupLanguage)

-- ------ begin <<tangle-imports>>[0]
import Attributes (attributes)
-- ------ end
-- ------ begin <<tangle-imports>>[1]
import ListStream (parseLine, parseLineNot, tokenLine)
-- ------ end
-- ------ begin <<tangle-imports>>[2]
import TextUtil (indent, unlines')
-- ------ end
-- ------ begin <<tangle-imports>>[3]
-- import Comment (annotateComment)
-- ------ end

-- ------ begin <<parse-markdown>>[0]
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
-- ------ end
-- ------ begin <<parse-markdown>>[1]
getLanguage :: ( MonadReader Config m )
            => [CodeProperty] -> m ProgrammingLanguage
getLanguage [] = return NoLanguage
getLanguage (CodeClass cls : _)
    = maybe (UnknownClass cls) 
            KnownLanguage
            <$> reader (\cfg -> lookupLanguage cfg cls)
getLanguage (_ : xs) = getLanguage xs
-- ------ end
-- ------ begin <<parse-markdown>>[2]
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
-- ------ end
-- ------ begin <<parse-markdown>>[3]
getFilePath :: [CodeProperty] -> Maybe FilePath
getFilePath [] = Nothing
getFilePath (CodeAttribute k v:xs)
    | k == "file" = Just $ T.unpack v
    | otherwise   = getFilePath xs
getFilePath (_:xs) = getFilePath xs

getFileMap :: [ReferencePair] -> FileMap
getFileMap = M.fromList . catMaybes . map filePair
    where filePair (ref, block) = do
              path <- getFilePath $ codeProperties block
              return (path, referenceName ref)
-- ------ end
-- ------ begin <<parse-markdown>>[4]
type ReferencePair = (ReferenceId, CodeBlock)

type ReferenceCount = Map ReferenceName Int

countReference :: ( MonadState ReferenceCount m )
               => ReferenceName -> m Int
countReference r = do
    x <- gets (M.findWithDefault 0 r)
    modify (M.insert r (x + 1))
    return x

newReference :: ( MonadState ReferenceCount m )
             => ReferenceName -> m ReferenceId
newReference n = ReferenceId n <$> countReference n
-- ------ end
-- ------ begin <<parse-markdown>>[5]
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
    ref'           <- getReference props
    return $ case ref' of
        Nothing  -> ( [ PlainText $ unlines' [begin, code, end] ], [] )
        Just ref -> ( [ PlainText begin, Reference ref, PlainText end ]
                    , [ ( ref, CodeBlock language props code ) ] )

normalText :: ( MonadParsec e (ListStream Text) m )
           => m ([Content], [ReferencePair])
normalText = do
    text <- unlines' <$> some (tokenLine (parseLineNot codeHeader))
    return ( [ PlainText text ], [] )

markdown :: DocumentParser ([Content], [ReferencePair])
markdown = mconcat <$> many (codeBlock <|> normalText)

parseMarkdown :: ( MonadReader Config m )
              => FilePath -> Text -> m (Either EntangledError Document)
parseMarkdown f t = do
    cfg <- ask
    let result' = parse (evalStateT (runReaderT markdown cfg) mempty) f (ListStream $ T.lines t)
    return $ case result' of
        Left err              -> Left (TangleError $ T.pack $ show err)
        Right (content, refs) -> Right $
            Document (M.fromList refs)
                     content
                     (getFileMap refs)
-- ------ end
-- ------ begin <<generate-code>>[0]
data CodeLine = PlainCode Text
              | NowebReference ReferenceName Text
              deriving (Eq, Show)

type CodeParser = Parsec Void Text
-- ------ end
-- ------ begin <<generate-code>>[1]
nowebReference :: CodeParser CodeLine
nowebReference = do
    indent <- takeWhileP Nothing (`elem` (" \t" :: [Char]))
    chunk "<<"
    id <- takeWhile1P Nothing (`notElem` (" \t<>" :: [Char]))
    chunk ">>"
    space >> eof
    return $ NowebReference (ReferenceName id) indent

parseCode :: ReferenceName -> Text -> [CodeLine]
parseCode name = map parseLine . T.lines
    where parseLine l = either (const $ PlainCode l) id
                      $ parse nowebReference
                              (T.unpack $ unReferenceName name)
                              l
-- ------ end
-- ------ begin <<generate-code>>[2]
type ExpandedCode = LM.Map ReferenceName (Either EntangledError Text)
type Annotator = Document -> ReferenceId -> (Either EntangledError Text)
-- ------ end
-- ------ begin <<generate-code>>[3]
expandedCode :: Annotator -> Document -> ExpandedCode
expandedCode annotate doc = result
    where result = LM.fromSet expand (referenceNames doc)
          expand name = unlines' <$> (sequence
                        $ map (annotate doc >=> expandCodeSource result name)
                        $ referencesByName doc name)

expandCodeSource :: ExpandedCode -> ReferenceName -> Text
                 -> Either EntangledError Text
expandCodeSource result name t
    = unlines' <$> sequence (map codeLineToText $ parseCode name t)
    where codeLineToText (PlainCode x) = Right x
          codeLineToText (NowebReference name i)
              = indent i <$> result LM.! name
-- ------ end
-- ------ begin <<generate-code>>[4]
annotateNaked :: Document -> ReferenceId -> Either EntangledError Text
annotateNaked doc ref = Right $ codeSource $ references doc M.! ref
-- ------ end
-- ------ end
