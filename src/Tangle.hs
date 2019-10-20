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
    , noneOf, chunk, many, some, endBy, eof, token
    , manyTill, anySingle, try, lookAhead, takeWhile1P, takeWhileP
    , (<|>), (<?>) )
import Text.Megaparsec.Char
    ( space )
import Data.Void
-- ------ end

import Control.Monad.Reader (MonadReader, reader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, gets, modify, StateT, evalStateT)
import Control.Monad.Fail (MonadFail)

import Data.Maybe (catMaybes)

import ListStream (ListStream (..))
import Document
    ( CodeProperty (..), CodeBlock (..), Content (..), ReferenceId (..)
    , ReferenceName (..), ProgrammingLanguage (..), Document (..), FileMap, unlines'
    , EntangledError (..) )
import Config (Config, languageName, lookupLanguage)

-- ------ begin <<parse-markdown>>[0]

-- ------ end
-- ------ begin <<parse-markdown>>[1]
codeHeader :: (MonadParsec e Text m)
           => m [CodeProperty]
codeHeader = do
    chunk "```" >> space >> chunk "{" >> space
    props <- (  codeClass
            <|> codeId
            <|> codeAttribute
             ) `endBy` space
    chunk "}" >> space >> eof
    return props 

codeFooter :: (MonadParsec e Text m)
           => m ()
codeFooter = chunk "```" >> space >> eof
-- ------ end
-- ------ begin <<parse-markdown>>[2]
cssIdentifier :: (MonadParsec e Text m)
              => m Text
cssIdentifier = takeWhile1P (Just "identifier")
                            (\c -> notElem c (" {}=" :: String))

cssValue :: (MonadParsec e Text m)
         => m Text
cssValue = takeWhileP (Just "value")
                      (\c -> notElem c (" {}=" :: String))

codeClass :: (MonadParsec e Text m)
          => m CodeProperty
codeClass = do
    chunk "."
    CodeClass <$> cssIdentifier
-- ------ end
-- ------ begin <<parse-markdown>>[3]
codeId :: (MonadParsec e Text m)
       => m CodeProperty
codeId = do
    chunk "#"
    CodeId <$> cssIdentifier
-- ------ end
-- ------ begin <<parse-markdown>>[4]
codeAttribute :: (MonadParsec e Text m)
              => m CodeProperty
codeAttribute = do
    key <- cssIdentifier
    chunk "="
    value <- cssValue
    return $ CodeAttribute key value
-- ------ end
-- ------ begin <<parse-markdown>>[5]
getLanguage :: ( MonadReader Config m )
            => [CodeProperty] -> m ProgrammingLanguage
getLanguage [] = return NoLanguage
getLanguage (CodeClass cls : _)
    = maybe (UnknownClass cls) 
            KnownLanguage
            <$> reader (lookupLanguage cls)
getLanguage (_ : xs) = getLanguage xs
-- ------ end
-- ------ begin <<parse-markdown>>[6]
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
-- ------ begin <<parse-markdown>>[7]
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
-- ------ begin <<parse-markdown>>[8]
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
-- ------ begin <<parse-markdown>>[9]
type LineParser = Parsec Void Text
type DocumentParser = ReaderT Config (StateT ReferenceCount (Parsec Void (ListStream Text)))

parseLine :: LineParser a -> Text -> Maybe (a, Text)
parseLine p t = either (const Nothing) (\x -> Just (x, t))
              $ parse p "" t

parseLineNot :: (Monoid a) => LineParser a -> Text -> Maybe Text
parseLineNot p t = either (const $ Just t) (const Nothing)
                 $ parse p "" t

tokenLine :: ( MonadParsec e (ListStream Text) m )
          => (Text -> Maybe a) -> m a
tokenLine f = token f mempty
-- ------ end
-- ------ begin <<parse-markdown>>[10]
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
indent :: Text -> Text -> Text
indent pre text
    = unlines' 
    $ map indentLine
    $ T.lines text
    where indentLine line
        | line == "" = line
        | otherwise  = T.append (T.pack indent) line
-- ------ end
-- ------ begin <<generate-code>>[1]
data CodeLine = PlainCode Text
              | NowebReference ReferenceName Text
              deriving (Eq, Show)

type CodeParser = Parsec Void Text
-- ------ end
-- ------ begin <<generate-code>>[2]
nowebReference :: CodeParser CodeLine
nowebReference = do
    indent <- space
    string "<<"
    id <- takeWhile1P Nothing (`notElem` " \t<>")
    string ">>"
    space >> eof
    return $ NowebReference (ReferenceName id) indent

parseCode :: ReferenceName -> Text -> [CodeLine]
parseCode name = map parseLine . T.lines
    where parseLine l = either (const $ PlainCode l) id
                      $ parse nowebReference
                              (T.unpack $ unReferenceName name)
                              l
-- ------ end
-- ------ begin <<generate-code>>[3]
type Dependencies = [ReferenceName]
type ExpandedCode = LM.Map ReferenceName Text
type Annotator = Document -> ReferenceId -> Text
-- ------ end
-- ------ begin <<generate-code>>[4]
expandedCode :: Annotator -> Document -> ExpandedCode
expandedCode annotate doc = result
    where result = LM.fromSet (referenceNames doc) expand
          expand name = unlines'
                        $ map (expandCodeSource result name)
                        $ map (annotate doc)
                        $ referencesByName doc name

expandCodeSource :: ExpandedCode -> ReferenceName -> Text -> Text
expandCodeSource result name t = codeLinesToText result $ parseCode name t
-- ------ end
-- ------ begin <<generate-code>>[5]
codeLineToText :: ExpandedCode -> CodeLine -> Text
codeLineToText _      (PlainCode x) = Right x
codeLineToText result (NowebReference name i) = indent i <$> result LM.! name
-- ------ end
-- ------ begin <<generate-code>>[6]
codeLinesToText :: ExpandedCode -> [CodeLine] -> Text
codeLinesToText result code = unlines' $ map (codeLineToText result) code
-- ------ end
-- ------ begin <<generate-code>>[7]
annotateNaked :: Document -> ReferenceId -> Either EntangledError Text
annotateNaked doc ref = Right $ codeSource $ references doc M.! ref
-- ------ end
-- ------ begin <<generate-code>>[8]
comment :: ProgrammingLanguage
        -> Text
        -> m (Either EntangledError Text)
comment (UnknownClass cls) _ = Left $ UnknownLanguageClass cls
comment NoLanguage         _ = Left $ MissingLanguageClass
comment (KnownLanguage lang) text = Right $ pre <> text <> post
    where pre  = languageStartComment lang <> "###"
          post = "###" <> languageCloseComment lang

annotateComment :: Document -> ReferenceId -> Either EntangledError Text
annotateComment doc ref = do
    let code = references doc M.! ref
    pre <- comment (codeLanguage code)
           $ " begin <<" <> (referenceName ref) <> ">>["
           <> T.pack (show $ referenceCount ref) <> "] "
    post <- comment (codeLanguage code) " end "
    return $ unlines' [pre, (codeSource code), post]
-- ------ end
-- ------ end
