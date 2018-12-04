module Parser
    ( parseMarkdown
    , parserSpec
    ) where

import Model
import Config
import Languages

import qualified Data.Map as Map
import Data.Either
-- import Data.Either.Combinators
import Data.Maybe
import Control.Monad
import Control.Monad.Reader

import qualified Text.Parsec as Parsec
import Text.Parsec (
      manyTill, anyChar, try, string, many1, many, noneOf, spaces, endBy, (<|>)
    , endOfLine, lookAhead, newline)

import Test.Hspec
import Data.Either.Combinators (fromRight')

data CodeProperty
    = CodeId String
    | CodeAttribute String String
    | CodeClass String
    deriving (Eq, Show)

type Parser = Parsec.ParsecT String ReferenceMap (Reader Config)

parse :: Parser a -> String -> String -> Reader Config (Either Parsec.ParseError a)
parse p = Parsec.runParserT p emptyReferenceMap

parse' :: Parser a -> String -> String -> Either Parsec.ParseError a
parse' p n t = runReader (parse p n t) defaultConfig

codeClass :: Parser CodeProperty
codeClass = do
    string "."
    cls <- many1 $ noneOf " {}"
    return $ CodeClass cls

codeId :: Parser CodeProperty
codeId = do
    string "#"
    id <- many1 $ noneOf " {}"
    return $ CodeId id

codeAttribute :: Parser CodeProperty
codeAttribute = do
    tag <- many1 $ noneOf " ={}"
    string "="
    value <- many1 $ noneOf " }"
    return $ CodeAttribute tag value

codeProperties :: Parser [CodeProperty]
codeProperties = do
    string "{"
    spaces
    props <- (codeClass <|> codeId <|> codeAttribute)
        `endBy` spaces
    string "}"
    return props

getLanguage :: [CodeProperty] -> Parser (Maybe Language)
getLanguage [] = return Nothing
getLanguage (CodeClass cls : _) = reader $ lookupLanguage cls
getLanguage (_ : xs) = getLanguage xs

getReferenceId :: ReferenceMap -> [CodeProperty] -> Maybe ReferenceID
getReferenceId _    [] = Nothing
getReferenceId refs (CodeAttribute tag value : xs) =
    case tag of
      "file"    -> Just $ FileReferenceID value
      _         -> getReferenceId refs xs
getReferenceId refs (CodeId id : _) =
    Just $ NameReferenceID id $ countReferences id refs
getReferenceId refs (_ : xs) = getReferenceId refs xs

line :: Parser String
line = manyTill anyChar (try endOfLine)

makeCodeBlock :: [CodeProperty] -> String -> Parser CodeBlock
makeCodeBlock props content = do
    language <- getLanguage props
    let languageId = maybe "plain" languageName language
    return $ CodeBlock languageId content

appendCode :: CodeBlock -> CodeBlock -> CodeBlock
appendCode (CodeBlock l t1) (CodeBlock _ t2) =
    CodeBlock l $ t1 ++ "\n" ++ t2

addCodeBlock :: CodeBlock -> ReferenceID -> ReferenceMap -> ReferenceMap
addCodeBlock code = Map.alter $ Just . maybe code (`appendCode` code)

codeBlock :: Parser [Content]
codeBlock = do
    opening <- lookAhead line
    string "```"
    spaces
    props <- codeProperties
    newline
    content <- manyTill anyChar (try $ string "\n```\n")
    block <- makeCodeBlock props content
    refs <- Parsec.getState
    case getReferenceId refs props of
      Nothing -> return [RawText opening, RawText content, RawText "```"]
      Just x  -> do
        Parsec.modifyState $ addCodeBlock block x
        return [RawText opening, Reference x, RawText "```"]

textLine :: Parser [Content]
textLine = do
    x <- line
    return [RawText x]

stripCodeBlocks :: Parser Document
stripCodeBlocks = do
    content <- many (try codeBlock <|> textLine)
    refmap  <- Parsec.getState
    return $ Document refmap (concat content)

parseMarkdown :: String -> String -> Reader Config (Either Parsec.ParseError Document)
parseMarkdown = Parsec.runParserT stripCodeBlocks emptyReferenceMap

parserSpec :: Spec
parserSpec =
  describe "Parser.FencedCodeBlocks" $ do
    it "can identify a reference id" $
      parse' codeId "" "#identifier" `shouldBe`
        (Right $ CodeId "identifier")

    it "can identify a code tag" $
      parse' codeAttribute "" "file=hello.c" `shouldBe`
        (Right $ CodeAttribute "file" "hello.c")

    it "can identify a class id" $
      parse' codeClass "" ".fortran77" `shouldBe`
        (Right $ CodeClass "fortran77")

    it "can get all code properties" $ do
      parse' codeProperties "" "{.cpp file=main.cc}" `shouldBe`
        Right [CodeClass "cpp", CodeAttribute "file" "main.cc"]
      parse' codeProperties "" "{.py #aaargh}" `shouldBe`
        Right [CodeClass "py", CodeId "aaargh"]

    it "can get a reference id" $
      getReferenceId emptyReferenceMap (fromRight' $ parse' codeProperties "" "{.scheme file=r6rs.scm}") `shouldBe`
        (Just $ FileReferenceID "r6rs.scm")
