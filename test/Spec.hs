{-# LANGUAGE FlexibleContexts #-}

module Main where

import Test.Hspec
import qualified Text.Parsec as Parsec
import qualified Data.Map as Map

import Data.List
import Data.Either
import Data.Either.Combinators (fromRight')
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import System.Random

import Model
import Markdown
import Tangle
import Untangle
import Config
import Daemon
import Document

parseMarkdown'' :: T.Text -> IO (Either TangleError Document)
parseMarkdown'' t = do
    randomGen <- getStdGen
    runReaderT (fst <$> runStateT (parseMarkdown "" t) randomGen) defaultConfig

markdownSpecs :: Map.Map FilePath (T.Text, Document) -> Spec
markdownSpecs lib = do
    describe "Parser.parseMarkdown" $ 
        context "when parsing 'test/test01.md'" $ do
            let (source, x) = lib Map.! "test/test01.md"
                refMap = references x
            it "has a file reference to 'hello.cc'" $
                refMap `shouldSatisfy` (\m -> FileReferenceId "hello.cc" `Map.member` m)

            let CodeBlock lang _ code pos = refMap Map.! FileReferenceId "hello.cc"
            it "has a codeblock in c++" $
                lang `shouldBe` "C++"

            it "is exactly the same when stitched together" $
                stitchText x `shouldBe` source

    describe "Tangle.tangleNaked" $ do
        context "when tangling result of 'test/test01.md'" $ do
            let (source, x) = lib Map.! "test/test01.md"
                t      = tangleNaked x
            
            it "has a single entry 'hello.cc'" $
                "hello.cc" `Map.member` t `shouldBe` True
            
            it "and this entry is a string" $
                t Map.! "hello.cc" `shouldSatisfy` isRight

            let code = fromRight' $ t Map.! "hello.cc"
            it "and this string has 7 lines" $
                length (T.lines code) `shouldBe` 7

        context "comparing results of 'test/test{n}.md'" $ do
            let t1 = tangleNaked $ snd $ lib Map.! "test/test01.md"
                t2 = tangleNaked $ snd $ lib Map.! "test/test02.md"
                t3 = tangleNaked $ snd $ lib Map.! "test/test03.md"
            it "content of 'hello.cc' is identical" $ do
                let c1 = fromRight' $ t1 Map.! "hello.cc"
                    c2 = fromRight' $ t2 Map.! "hello.cc"
                    c3 = fromRight' $ t3 Map.! "hello.cc"
                c2 `shouldBe` c1
                c3 `shouldBe` c1

        context "when tangling 'test/test_cycle.md'" $ do
            let (source, x) = lib Map.! "test/test_cycle.md"
                t = tangleNaked x
            it "should result in an error" $
                t Map.! "loop.cc" `shouldSatisfy` isCyclicReference

    describe "Lib" $
        context "when untangling the tangled" $ do
            let doc = snd $ lib Map.! "test/test03.md"
                rmo = references doc
                fm  = runReader (tangleAnnotated $ references doc) defaultConfig
            it "succeeds at tangling" $
                fm Map.! "hello.cc" `shouldSatisfy` isRight
            let code = fromRight' $ fm Map.! "hello.cc"
                rm' = runReader (untangle "hello.cc" (T.unpack code)) defaultConfig
            it "succeeds at untangling" $
                rm' `shouldSatisfy` isRight
            let rm  = fromRight' rm'
            it "recovers the identical code block" $ do
                let ref1 = FileReferenceId "hello.cc"
                    ref2 = NameReferenceId "main-body" 0
                withoutLineDirectives (codeSource (rm Map.! ref1)) `shouldBe` withoutLineDirectives (codeSource (rmo Map.! ref1))
                withoutLineDirectives (codeSource (rm Map.! ref2)) `shouldBe` withoutLineDirectives (codeSource (rmo Map.! ref2))
            
withoutLineDirectives = T.unlines . filter (not . T.isPrefixOf (T.pack "#LINE")) . T.lines

spec :: Map.Map FilePath (T.Text, Document) -> Spec
spec lib = do
    tangleSpec
    untangleSpec
    markdownSpecs lib

{- These files are read and tested against. -}
testFiles = ["test/test01.md", "test/test02.md", "test/test03.md", "test/test04.md", "test/test_cycle.md"]

main :: IO ()
main = do
    lib <- Map.fromList . zip testFiles 
        <$> mapM (\ f -> do { t <- T.IO.readFile f; d <- fromRight' <$> parseMarkdown'' t; return (t, d) }) testFiles
    hspec $ spec lib
