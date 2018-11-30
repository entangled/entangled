module Main where

import Test.Hspec
import Text.Parsec
import qualified Data.Map as Map

import Model
import Parser
import Data.List
import Data.Either.Combinators
import Data.Maybe
import Control.Monad.IO.Class

parserSpec :: Spec
parserSpec =
  describe "Parser.FencedCodeBlocks" $ do
    it "can identify a reference id" $
      parse codeId "" "#identifier" `shouldBe`
        (Right $ CodeId "identifier")

    it "can identify a code tag" $
      parse codeAttribute "" "file=hello.c" `shouldBe`
        (Right $ CodeAttribute "file" "hello.c")

    it "can identify a class id" $
      parse codeClass "" ".fortran77" `shouldBe`
        (Right $ CodeClass "fortran77")

    it "can get all code properties" $ do
      parse codeProperties "" "{.cpp file=main.cc}" `shouldBe`
        Right [CodeClass "cpp", CodeAttribute "file" "main.cc"]
      parse codeProperties "" "{.py #aaargh}" `shouldBe`
        Right [CodeClass "py", CodeId "aaargh"]

    it "can extract the language" $
      getLanguage (fromRight' $ parse codeProperties "" "{.scheme file=r6rs.scm}") `shouldBe`
        (Just $ Language "scheme")

    it "can get a reference id" $
      getReferenceId (fromRight' $ parse codeProperties "" "{.scheme file=r6rs.scm}") `shouldBe`
        (Just $ FileReferenceID "r6rs.scm")

tangleSpecs :: Spec
tangleSpecs =
    describe "Tangle."
    
markdownSpecs :: Map.Map String String -> Spec
markdownSpecs lib =
  describe "Parser.parseMarkdown" $ 
    context "when parsing 'test/test01.md'" $ do
        let source = lib Map.! "test/test01.md"
            x'      = parseMarkdown source
        it "can parse the text" $
            x' `shouldSatisfy` isRight

        let x      = fromRight' x'
            refMap = references x
        it "has a file reference to 'hello.cc'" $
            refMap `shouldSatisfy` (\m -> FileReferenceID "hello.cc" `Map.member` m)

        let CodeBlock lang code = refMap Map.! FileReferenceID "hello.cc"
        it "has a codeblock in c++" $
            lang `shouldBe` Language "cpp"

        it "is exactly the same when stitched together" $
            stitchText x `shouldBe` source

spec :: Map.Map String String -> Spec
spec lib = do
  parserSpec
  markdownSpecs lib

{- These files are read and tested against. -}
testFiles = ["test/test01.md"]

main :: IO ()
main = do
    lib <- Map.fromList . zip testFiles <$> mapM readFile testFiles
    hspec $ spec lib
