-- ------ language="Haskell" file="test/TangleSpec.hs"
module TangleSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end

import Tangle
import Document
    ( CodeProperty(..)
    , Content(..)
    , ReferenceId(..)
    , ProgrammingLanguage(..)
    , CodeBlock(..) )
import Config (Config, defaultConfig)
import ListStream

import Text.Megaparsec (parse, Parsec)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)
import Control.Monad.Reader (ReaderT, runReaderT)

type Parser = Parsec Void Text

p :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
p x t = parse x "" t

type ParserC = ReaderT Config (Parsec Void (ListStream Text))

pc :: ParserC a -> ListStream Text -> Either (ParseErrorBundle (ListStream Text) Void) a
pc x t = parse (runReaderT x defaultConfig) "" t

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
        it "parses a code block" $ do
            pc codeBlock test1 `shouldParse`
                ( [ PlainText "``` {.python #hello-world}"
                  , Reference (NameReferenceId "hello-world" 0)
                  , PlainText "```" ]
                , [ ( NameReferenceId "hello-world" 0
                    , CodeBlock (KnownLanguage "Python")
                                [CodeClass "python", CodeId "hello-world"]
                                "print(\"Hello, World!\")"
                    ) ]
                )
-- ------ end
