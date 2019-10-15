-- ------ language="Haskell" file="app/Tangle.hs"
module Tangle where

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ begin <<import-megaparsec>>[0]
import Text.MegaParsec (ParsecT)
-- ------ end

import ListStream (ListStream (..))

-- ------ begin <<parse-markdown>>[0]
data ParserT = ParsecT Void (ListStream Text)

parseDocument :: ( MonadState ReferenceMap m )
              => ParserT m Document
parseDocument = return empty
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
data LineParser = Parsec Void Text
data DocumentParserT m = ParsecT Void [Text] (StateT ReferenceMap m)

parseLine :: LineParser a -> Text -> Maybe (a, Text)
parseLine p t = either (const Nothing) (\x -> Just (x, t))
              $ parse p "" t

parseLineNot :: LineParser a -> Text -> Maybe Text
parseLineNot p t = either (const $ Just t) (const Nothing)
                 $ parse p "" t

tokenLine :: ( MonadParsec e [Text] m )
          => LineParser -> m (a, Text)
tokenLine f = token f empty

codeBlock :: ( MonadParsec e [Text] m,
               MonadReader Config m )
          => m ([Content], [ReferencePair])
codeBlock = do
    (begin, props) <- tokenLine (parseLine codeHeader)
    code           <- unlines' <$> manyTill (anySingle <?> "code line")
                                            (try $ lookAhead $ tokenLine (parseLine codeFooter))
    (end, _)       <- tokenLine (parseLine codeFooter)
    language       <- getLanguage props
    return $ case getReference props of
        Nothing  -> ( [ PlainText $ unlines' [begin, code, end] ], [] )
        Just ref -> ( [ PlainText begin, ref, PlainText end ]
                    , [ ( ref, CodeBlock language props code ) ] )

normalText :: ( MonadParsec e [Text] m )
           => m ([Content], [ReferencePair])
normalText = do
    text <- unlines' <$> many1 (tokenLine (parseLineNot codeHeader))
    return ( [ PlainText text ], [] )
-- ------ end
-- ------ begin <<parse-markdown>>[3]
getLanguage :: ( MonadReader Config m )
            => [CodeProperty] -> m ProgrammingLanguage
getLanguage [] = return NoLanguage
getLanguage (CodeClass cls : _)
    = maybe (UnknownLanguage cls) 
            (KnownLanguage . languageName)
            <$> reader (lookupLanguage cls)
getLanguage (_ : xs) = getLanguage xs
-- ------ end
-- ------ begin <<parse-markdown>>[4]
codeClass :: (MonadParsec e Text m)
          => m CodeProperty
codeClass = do
    chunk "."
    CodeClass <$> many1 (noneOf " {}")
-- ------ end
-- ------ begin <<parse-markdown>>[5]
codeId :: (MonadParsec e Text m)
       => m CodeProperty
codeId = do
    chunk "#"
    CodeId <$> many1 (noneOf " {}")
-- ------ end
-- ------ begin <<parse-markdown>>[6]
codeAttribute :: (MonadParsec e Text m)
              => m CodeAttribute
codeAttribute = do
    key <- many1 (noneOf " ={}")
    chunk "="
    value <- many1 (noneOf " {}")
    return $ CodeAttribute key value
-- ------ end
-- ------ end
