module Main where

import Test.Hspec
import Text.Parsec
import qualified Data.Map as Map

import Data.List
import Data.Either.Combinators
import Data.Maybe
import Control.Monad.IO.Class

import Model
import Parser
import Tangle

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
      getLanguageId (fromRight' $ parse codeProperties "" "{.scheme file=r6rs.scm}") `shouldBe`
        (Just $ LanguageId "scheme")

    it "can get a reference id" $
      getReferenceId (fromRight' $ parse codeProperties "" "{.scheme file=r6rs.scm}") `shouldBe`
        (Just $ FileReferenceID "r6rs.scm")

tangleSpec :: Spec
tangleSpec = do
    describe "Tangle.codeLine" $
        it "just separates lines" $
            parse (many codeLine) "" "Hello\nWorld\n" `shouldBe`
              Right [SourceText "Hello", SourceText "World"]
    describe "Tangle.nowebReference" $ do
        it "picks out reference" $
            parse nowebReference "" "  <<test>>\n" `shouldBe`
              (Right $ NowebReference "test" "  ")
        it "skips anything that doesn't match" $
            parse nowebReference "" "meh <<test>>\n" `shouldSatisfy`
              isLeft
    
markdownSpecs :: Map.Map String String -> Spec
markdownSpecs lib = do
  describe "Parser.parseMarkdown" $ 
    context "when parsing 'test/test01.md'" $ do
        let source = lib Map.! "test/test01.md"
            x'     = parseMarkdown source
        it "can parse the text" $
            x' `shouldSatisfy` isRight

        let x      = fromRight' x'
            refMap = references x
        it "has a file reference to 'hello.cc'" $
            refMap `shouldSatisfy` (\m -> FileReferenceID "hello.cc" `Map.member` m)

        let CodeBlock lang code = refMap Map.! FileReferenceID "hello.cc"
        it "has a codeblock in c++" $
            lang `shouldBe` LanguageId "cpp"

        it "is exactly the same when stitched together" $
            stitchText x `shouldBe` source

  describe "Tangle.tangleNaked" $ do
     context "when tangling result of 'test/test01.md'" $ do
        let source = lib Map.! "test/test01.md"
            x      = fromRight' $ parseMarkdown source
            t      = tangleNaked x
        
        it "has a single entry 'hello.cc'" $
            "hello.cc" `Map.member` t `shouldBe` True
        
        it "and this entry is a string" $
            t Map.! "hello.cc" `shouldSatisfy` isRight

        let code = fromRight' $ t Map.! "hello.cc"
        it "and this string has 7 lines" $
            length (lines code) `shouldBe` 7
    
     context "comparing results of 'test/test{n}.md'" $ do
        let t1 = tangleNaked $ fromRight' $ parseMarkdown $ lib Map.! "test/test01.md"
            t2 = tangleNaked $ fromRight' $ parseMarkdown $ lib Map.! "test/test02.md"
            t3 = tangleNaked $ fromRight' $ parseMarkdown $ lib Map.! "test/test03.md"
        it "content of 'hello.cc' is identical" $ do
            let c1 = fromRight' $ t1 Map.! "hello.cc"
                c2 = fromRight' $ t2 Map.! "hello.cc"
                c3 = fromRight' $ t3 Map.! "hello.cc"
            c2 `shouldBe` c1
            c3 `shouldBe` c1

spec :: Map.Map String String -> Spec
spec lib = do
  parserSpec
  tangleSpec
  markdownSpecs lib

{- These files are read and tested against. -}
testFiles = ["test/test01.md", "test/test02.md", "test/test03.md"]

main :: IO ()
main = do
    lib <- Map.fromList . zip testFiles <$> mapM readFile testFiles
    hspec $ spec lib
