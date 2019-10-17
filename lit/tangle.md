# Tangling

``` {.haskell file=src/Tangle.hs}
module Tangle where

<<import-text>>
<<import-megaparsec>>

import Control.Monad.Reader (MonadReader, reader)

import ListStream (ListStream (..))
import Document
    ( CodeProperty (..), CodeBlock (..), Content (..), ReferenceId (..)
    , ReferencePair, ProgrammingLanguage (..), unlines')
import Config (Config, languageName, lookupLanguage)

<<parse-markdown>>
```

The task of tangling means:

* Parse the markdown to a `Document`
* Generate annotated source files.

Parsing the markdown is done on a line per line basis. We don't try to parse the markdown itself, rather we try to detect lines that start and end code-blocks.

Remember the golden rule:

> Untangling from a generated source returns the same markdown **to the byte**.

We will be adding unit tests to check exactly this property.

## Quasi-parsing Markdown

Parsing the markdown using MegaParsec,

``` {.haskell #parse-markdown}
```

### Parsing code blocks
Code blocks are delimited with three back-quotes, and again closed with three back-quotes. Pandoc allows for any number larger than three, and using other symbols like tildes. The only form Entangled reacts to is the following **unindented** template: 

~~~markdown
 ``` {[#<reference>|.<language>|<key>=<value>] ...}
 <code> ...
 ```
~~~

Any code block is one of the following:
    
* A **referable** block: has exactly one **reference id** (`#<reference>`) and exactly one class giving the **language** of the code block (`.<language>`). Example:

~~~markdown
 ``` {.rust #hello-rust}
 println!("Hello, Rust!");
 ```
~~~

* A **file** block: has no reference id, but a key-value pair giving the path to the file (`file=<path>`), absolute or relative to the directory in which `entangled` runs. Again, there should be exactly one class giving the **language** of the block (`.<language>`). Example:

~~~markdown
 ``` {.rust file=src/main.rs}
 fn main() {
    <<hello-rust>>
 }
 ```
~~~

* An **ignored** block: anything not matching the previous two.

## Parsing single lines

Written using parser combinators.

``` {.haskell #parse-markdown}
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
```

## Parsing the document

Using these two parsers, we can create a larger parser that works on a line-by-line basis.

``` {.haskell #parse-markdown}
type LineParser = Parsec Void Text
-- data DocumentParserT m = ParsecT Void [Text] (StateT ReferenceMap m)

parseLine :: LineParser a -> Text -> Maybe (a, Text)
parseLine p t = either (const Nothing) (\x -> Just (x, t))
              $ parse p "" t

parseLineNot :: (Monoid a) => LineParser a -> Text -> Maybe Text
parseLineNot p t = either (const $ Just t) (const Nothing)
                 $ parse p "" t

tokenLine :: ( MonadParsec e (ListStream Text) m )
          => (Text -> Maybe a) -> m a
tokenLine f = token f mempty

codeBlock :: ( MonadParsec e (ListStream Text) m,
               MonadReader Config m )
          => m ([Content], [ReferencePair])
codeBlock = do
    (props, begin) <- tokenLine (parseLine codeHeader)
    code           <- unlines' 
                   <$> manyTill (anySingle <?> "code line")
                                (try $ lookAhead $ tokenLine (parseLine codeFooter))
    (_, end)       <- tokenLine (parseLine codeFooter)
    language       <- getLanguage props
    return $ case getReference props of
        Nothing  -> ( [ PlainText $ unlines' [begin, code, end] ], [] )
        Just ref -> ( [ PlainText begin, Reference ref, PlainText end ]
                    , [ ( ref, CodeBlock language props code ) ] )

normalText :: ( MonadParsec e (ListStream Text) m )
           => m ([Content], [ReferencePair])
normalText = do
    text <- unlines' <$> some (tokenLine (parseLineNot codeHeader))
    return ( [ PlainText text ], [] )
```

In case the language is not given, or is misspelled, the system should be aware of that, so we can give an error message or warning. On the other hand, if an identifyer is left out, the parser should fail, so that the code-block is included as normal markdown.

``` {.haskell #parse-markdown}
getLanguage :: ( MonadReader Config m )
            => [CodeProperty] -> m ProgrammingLanguage
getLanguage [] = return NoLanguage
getLanguage (CodeClass cls : _)
    = maybe (UnknownClass cls) 
            (KnownLanguage . languageName)
            <$> reader (lookupLanguage cls)
getLanguage (_ : xs) = getLanguage xs

getReference :: [CodeProperty] -> Maybe ReferenceId
getReference [] = Nothing
getReference (CodeId x:_) = Just $ NameReferenceId x 0
getReference (CodeAttribute k v:xs)
    | k == "file" = Just $ FileReferenceId v
    | otherwise   = getReference xs
getReference (_:xs) = getReference xs
```

#### class
A class starts with a period (`.`), and cannot contain spaces or curly braces.

``` {.haskell #parse-markdown}
cssIdentifier :: (MonadParsec e Text m)
              => m Text
cssIdentifier = takeWhile1P (Just "identifier")
                            (\c -> notElem c (" {}=" :: String))

cssValue :: (MonadParsec e Text m)
         => m Text
cssValue = takeWhileP (Just "value")
                      (\c -> notElem c (" {}=" :: String))

codeClass :: (MonadParsec e Text m)
          => m CodeProperty
codeClass = do
    chunk "."
    CodeClass <$> cssIdentifier
```

#### id
An id starts with a hash (`#`), and cannot contain spaces or curly braces.

``` {.haskell #parse-markdown}
codeId :: (MonadParsec e Text m)
       => m CodeProperty
codeId = do
    chunk "#"
    CodeId <$> cssIdentifier
```

#### attribute
An generic attribute is written as key-value-pair, separated by an equals sign (`=`). Again no spaces or curly braces are allowed inside.

``` {.haskell #parse-markdown}
codeAttribute :: (MonadParsec e Text m)
              => m CodeProperty
codeAttribute = do
    key <- cssIdentifier
    chunk "="
    value <- cssValue
    return $ CodeAttribute key value
```


## Testing

``` {.haskell file=test/TangleSpec.hs}
module TangleSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

<<import-text>>

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
```

