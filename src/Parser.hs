module Parser
    (CodeProperty(..)
    , codeId
    , codeClass
    , codeAttribute
    , codeProperties
    , getLanguageId
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

getLanguageId :: [CodeProperty] -> Maybe LanguageId
getLanguageId [] = Nothing
getLanguageId (CodeClass cls : _) = Just $ LanguageId cls
getLanguageId (_ : xs) = getLanguageId xs

getReferenceId :: ReferenceMap -> [CodeProperty] -> Maybe ReferenceID
getReferenceId _    [] = Nothing
getReferenceId _    (CodeAttribute tag value : xs) =
    case tag of
      "file"    -> Just $ FileReferenceID value
      _         -> getReferenceId xs
getReferenceId refs (CodeId id : _) =
    Just $ NameReferenceID id $ countReferences refs id
getReferenceId refs (_ : xs) = getReferenceId refs xs

line :: Parsec String b String
line = manyTill anyChar (try endOfLine)

makeCodeBlock :: [CodeProperty] -> String -> CodeBlock
makeCodeBlock props = CodeBlock languageId
    where languageId = fromMaybe (LanguageId "plain") (getLanguageId props)

appendCode :: CodeBlock -> CodeBlock -> CodeBlock
appendCode (CodeBlock l t1) (CodeBlock _ t2) =
    CodeBlock l $ t1 ++ "\n" ++ t2

addCodeBlock :: CodeBlock -> ReferenceID -> ReferenceMap -> ReferenceMap
addCodeBlock code = Map.alter $ Just . maybe code (`appendCode` code)

codeBlock :: Parsec String ReferenceMap [Text]
codeBlock = do
    opening <- lookAhead line
    string "```"
    spaces
    props <- codeProperties
    newline
    content <- manyTill anyChar (try $ string "\n```\n")
    refs <- getState
    case getReferenceId refs props of
      Nothing -> return [RawText opening, RawText content, RawText "```"]
      Just x  -> do
        modifyState $ addCodeBlock (makeCodeBlock props content) x
        return [RawText opening, Reference x, RawText "```"]

textLine :: Parsec String b [Text]
textLine = do
    x <- line
    return [RawText x]

stripCodeBlocks :: Parsec String ReferenceMap Document
stripCodeBlocks = do
    content <- many (try codeBlock <|> textLine)
    refmap  <- getState
    return $ Document refmap (concat content)

parseMarkdown :: String -> Either ParseError Document
parseMarkdown = runParser stripCodeBlocks emptyReferenceMap ""

readMarkdown :: FilePath -> IO (Either ParseError Document)
readMarkdown path = do
    source <- readFile path
    return $ parseMarkdown source
