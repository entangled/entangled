-- ~\~ language=Haskell filename=test/TangleSpec.hs
-- ~\~ begin <<lit/13-tangle.md|test/TangleSpec.hs>>[0]
module TangleSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

-- ~\~ begin <<lit/01-entangled.md|import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ~\~ end
import qualified Data.Text.IO as T.IO

import qualified Data.Map.Lazy as LM
import Data.List ((!!))

import Tangle
import Attributes
import Document
import Config (Config, defaultConfig, languageFromName)
import ListStream

import Text.Megaparsec (parse, Parsec)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)
import Control.Monad.Reader (ReaderT, runReaderT, runReader)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<))

import Data.Maybe (fromJust)
import Data.Either (fromRight, rights)

type Parser = Parsec Void Text

p :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
p x t = parse x "" t

type ParserC = StateT ReferenceCount (ReaderT Config (Parsec Void (ListStream Text)))

pc :: ParserC a -> ListStream Text -> Either (ParseErrorBundle (ListStream Text) Void) a
pc x t = parse (runReaderT (evalStateT x mempty) defaultConfig) "" t

pd :: Text -> Either EntangledError Document
pd t = runReader (parseMarkdown "" t) defaultConfig

tangleSpec :: Spec
tangleSpec = do
    describe "Parsing fenced code attributes" $ do
        it "parses a class" $ do
            p codeClass ".hello" `shouldParse` CodeClass "hello"
            p codeClass `shouldFailOn` "nodot"
            p codeClass `shouldFailOn` ".===="
        it "parses an id" $ do
            p codeId "#world" `shouldParse` CodeId "world"
            p codeId `shouldFailOn` "nosharp"
            p codeId `shouldFailOn` "#{}{}{"
        it "parses an attribute" $ do
            p codeAttribute "file=hello.c" `shouldParse` CodeAttribute "file" "hello.c"
            p codeAttribute "empty= value" `shouldParse` CodeAttribute "empty" ""
            p codeAttribute `shouldFailOn` "{}="
        it "parses a header" $ do
            p codeHeader "``` {.class #id key=value}" `shouldParse`
                [CodeClass "class", CodeId "id", CodeAttribute "key" "value"]
            p codeHeader `shouldFailOn` "``` {key==blah}"
            p codeHeader `shouldFailOn` "``` {key}"
        it "respects and disrespects whitespace where needed" $ do
            p codeHeader "```{}" `shouldParse` []
            p codeHeader "```   {}  " `shouldParse` []
            p codeHeader `shouldFailOn` "   ```{}"

    describe "Parsing a code block" $ do
        let test1 = ListStream
                    [ "``` {.python #hello-world}"
                    , "print(\"Hello, World!\")"
                    , "```" ]
            python = fromJust $ languageFromName defaultConfig "Python"
        it "parses a code block" $ do
            pc codeBlock test1 `shouldParse`
                ( [ PlainText "``` {.python #hello-world}"
                  , Reference $ ReferenceId (ReferenceName "hello-world") 0
                  , PlainText "```" ]
                , [ ( ReferenceId (ReferenceName "hello-world") 0
                    , CodeBlock (KnownLanguage python)
                                [CodeClass "python", CodeId "hello-world"]
                                "print(\"Hello, World!\")"
                    ) ]
                )

tangleEqualSpec :: Spec
tangleEqualSpec = before (sequence $ map T.IO.readFile testFiles) $ do
    describe "Code expansion on test0[1-3].md" $ do
        it "expands to identical code" $ \files -> do
            let docs = rights $ map pd files
                codes = map (expandedCode annotateNaked) docs
                hellos = map (LM.! (ReferenceName "hello.cc")) codes
            hellos `shouldBe` (replicate 3 (hellos !! 0))
    where testFiles = ["test/test01.md", "test/test02.md", "test/test03.md"]
-- ~\~ end
