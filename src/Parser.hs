module Parser
    (CodeProperty(..)
    , codeId
    , codeClass
    , codeAttribute
    , codeProperties
    , getLanguage
    , getReferenceId
    , parseMarkdown
    , readMarkdown
    ) where

import Model
import qualified Data.Map as Map
import Data.Either
import Data.Either.Combinators
import Data.Maybe
import Text.Parsec
import Control.Monad

data CodeProperty
    = CodeId String
    | CodeAttribute String String
    | CodeClass String
    deriving (Eq, Show)

codeClass :: Parsec String b CodeProperty
codeClass = do
    string "."
    cls <- many1 $ noneOf " {}"
    return $ CodeClass cls

codeId :: Parsec String b CodeProperty
codeId = do
    string "#"
    id <- many1 $ noneOf " {}"
    return $ CodeId id

codeAttribute :: Parsec String b CodeProperty
codeAttribute = do
    tag <- many1 $ noneOf " ={}"
    string "="
    value <- many1 $ noneOf " }"
    return $ CodeAttribute tag value

codeProperties :: Parsec String b [CodeProperty]
codeProperties = do
    string "{"
    spaces
    props <- (codeClass <|> codeId <|> codeAttribute)
        `endBy` spaces
    string "}"
    return props

getLanguage :: [CodeProperty] -> Maybe Language
getLanguage [] = Nothing
getLanguage (CodeClass cls : _) = Just $ Language cls
getLanguage (_ : xs) = getLanguage xs

getReferenceId :: [CodeProperty] -> Maybe ReferenceID
getReferenceId [] = Nothing
getReferenceId (CodeAttribute tag value : xs) =
    case tag of
      "file"    -> Just $ FileReferenceID value
      _         -> getReferenceId xs
getReferenceId (CodeId id : _) = Just $ NameReferenceID id
getReferenceId (_ : xs) = getReferenceId xs

line :: Parsec String b String
line = manyTill anyChar (try endOfLine)

makeCodeBlock :: [CodeProperty] -> String -> CodeBlock
makeCodeBlock props = CodeBlock language
    where language = fromMaybe (Language "plain") (getLanguage props)

codeBlock :: Parsec String ReferenceMap [Text]
codeBlock = do
    opening <- lookAhead line
    string "```"
    spaces
    props <- codeProperties
    newline
    content <- manyTill anyChar (try $ string "\n```\n")
    case getReferenceId props of
      Nothing -> return [RawText opening, RawText content, RawText "```\n"]
      Just x  -> do
        modifyState $ Map.insert x $ makeCodeBlock props content
        return [RawText opening, Reference x, RawText "```"]

textLine :: Parsec String b [Text]
textLine = do
    x <- line
    return [RawText x]

stripCodeBlocks :: Parsec String ReferenceMap Document
stripCodeBlocks = do
    content <- many (codeBlock <|> textLine)
    refmap  <- getState
    return $ Document refmap (concat content)

parseMarkdown :: String -> Either ParseError Document
parseMarkdown = runParser stripCodeBlocks emptyReferenceMap ""

readMarkdown :: FilePath -> IO (Either ParseError Document)
readMarkdown path = do
    source <- readFile path
    return $ parseMarkdown source
