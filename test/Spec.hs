module Main where

import Test.Hspec
import qualified Text.Parsec as Parsec
import qualified Data.Map as Map

import Data.List
import Data.Either.Combinators
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad.Reader

import Model
import Parser
import Tangle
import Untangle
import Config

parseMarkdown' :: String -> Either Parsec.ParseError Document
parseMarkdown' t = runReader (parseMarkdown "" t) defaultConfig

markdownSpecs :: Map.Map String String -> Spec
markdownSpecs lib = do
    describe "Parser.parseMarkdown" $ 
        context "when parsing 'test/test01.md'" $ do
            let source = lib Map.! "test/test01.md"
                x'     = parseMarkdown' source
            it "can parse the text" $
                x' `shouldSatisfy` isRight

            let x      = fromRight' x'
                refMap = references x
            it "has a file reference to 'hello.cc'" $
                refMap `shouldSatisfy` (\m -> FileReferenceID "hello.cc" `Map.member` m)

            let CodeBlock lang code = refMap Map.! FileReferenceID "hello.cc"
            it "has a codeblock in c++" $
                lang `shouldBe` "C++"

            it "is exactly the same when stitched together" $
                stitchText x `shouldBe` source

    describe "Tangle.tangleNaked" $ do
        context "when tangling result of 'test/test01.md'" $ do
            let source = lib Map.! "test/test01.md"
                x      = fromRight' $ parseMarkdown' source
                t      = tangleNaked x
            
            it "has a single entry 'hello.cc'" $
                "hello.cc" `Map.member` t `shouldBe` True
            
            it "and this entry is a string" $
                t Map.! "hello.cc" `shouldSatisfy` isRight

            let code = fromRight' $ t Map.! "hello.cc"
            it "and this string has 7 lines" $
                length (lines code) `shouldBe` 7

        context "comparing results of 'test/test{n}.md'" $ do
            let t1 = tangleNaked $ fromRight' $ parseMarkdown' $ lib Map.! "test/test01.md"
                t2 = tangleNaked $ fromRight' $ parseMarkdown' $ lib Map.! "test/test02.md"
                t3 = tangleNaked $ fromRight' $ parseMarkdown' $ lib Map.! "test/test03.md"
            it "content of 'hello.cc' is identical" $ do
                let c1 = fromRight' $ t1 Map.! "hello.cc"
                    c2 = fromRight' $ t2 Map.! "hello.cc"
                    c3 = fromRight' $ t3 Map.! "hello.cc"
                c2 `shouldBe` c1
                c3 `shouldBe` c1

    describe "Lib" $
        context "when untangling the tangled" $ do
            let doc = fromRight' $ parseMarkdown' $ lib Map.! "test/test03.md"
                rmo = references doc
                fm  = runReader (tangleAnnotated doc) defaultConfig
            it "succeeds at tangling" $
                fm Map.! "hello.cc" `shouldSatisfy` isRight
            let code = fromRight' $ fm Map.! "hello.cc"
                rm' = runReader (untangle "hello.cc" code) defaultConfig
            it "succeeds at untangling" $
                rm' `shouldSatisfy` isRight
            let rm  = fromRight' rm'
            it "recovers the identical code block" $ do
                let ref1 = FileReferenceID "hello.cc"
                    ref2 = NameReferenceID "main-body" 0
                rm Map.! ref1 `shouldBe` rmo Map.! ref1
                rm Map.! ref2 `shouldBe` rmo Map.! ref2  
            

spec :: Map.Map String String -> Spec
spec lib = do
    parserSpec
    tangleSpec
    untangleSpec
    markdownSpecs lib

{- These files are read and tested against. -}
testFiles = ["test/test01.md", "test/test02.md", "test/test03.md"]

main :: IO ()
main = do
    lib <- Map.fromList . zip testFiles <$> mapM readFile testFiles
    hspec $ spec lib
