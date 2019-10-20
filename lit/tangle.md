# Tangling

``` {.haskell file=src/Tangle.hs}
module Tangle where

<<import-text>>
<<import-map>>
<<import-lazy-map>>

<<import-megaparsec>>

import Control.Monad.Reader (MonadReader, reader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, gets, modify, StateT, evalStateT)
import Control.Monad.Fail (MonadFail)

import Data.Maybe (catMaybes)

import ListStream (ListStream (..))
import Document
    ( CodeProperty (..), CodeBlock (..), Content (..), ReferenceId (..)
    , ReferenceName (..), ProgrammingLanguage (..), Document (..), FileMap, unlines'
    , EntangledError (..) )
import Config (Config, languageName, lookupLanguage)

<<parse-markdown>>
<<generate-code>>
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

* A **file** block: has a key-value pair giving the path to the file (`file=<path>`), absolute or relative to the directory in which `entangled` runs. Again, there should be exactly one class giving the **language** of the block (`.<language>`). Example:

~~~markdown
 ``` {.rust #main-function file=src/main.rs}
 fn main() {
    <<hello-rust>>
 }
 ```
~~~

* An **ignored** block: anything not matching the previous two.

### Parsing single lines

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

### Extracting data from a list of `CodeProperty`

In case the language is not given, or is misspelled, the system should be aware of that, so we can give an error message or warning.

``` {.haskell #parse-markdown}
getLanguage :: ( MonadReader Config m )
            => [CodeProperty] -> m ProgrammingLanguage
getLanguage [] = return NoLanguage
getLanguage (CodeClass cls : _)
    = maybe (UnknownClass cls) 
            KnownLanguage
            <$> reader (lookupLanguage cls)
getLanguage (_ : xs) = getLanguage xs
```

On the other hand, if an identifyer is left out, the parser should fail, so that the code-block is included as normal markdown. If only a file attribute is given, the filename is also the identifyer. If an id is also given, that takes precedence. We use a `StateT ReferenceCount` to keep track of the number of references with the same name, so that we can concatenate these code blocks in order.

``` {.haskell #parse-markdown}
getReference :: ( MonadState ReferenceCount m )
             => [CodeProperty] -> m (Maybe ReferenceId)
getReference [] = return Nothing
getReference (CodeId x:_) = Just <$> newReference (ReferenceName x)
getReference (CodeAttribute k v:xs)
    | k == "file" = do
        a <- getReference xs
        b <- Just <$> newReference (ReferenceName v)
        return $ a <|> b
    | otherwise   = getReference xs
getReference (_:xs) = getReference xs
```

We keep a separate `Map` to link certain references with top-level files.

``` {.haskell #parse-markdown}
getFilePath :: [CodeProperty] -> Maybe FilePath
getFilePath [] = Nothing
getFilePath (CodeAttribute k v:xs)
    | k == "file" = Just $ T.unpack v
    | otherwise   = getFilePath xs
getFilePath (_:xs) = getFilePath xs

getFileMap :: [ReferencePair] -> FileMap
getFileMap = M.fromList . catMaybes . map filePair
    where filePair (ref, block) = do
              path <- getFilePath $ codeProperties block
              return (path, referenceName ref)
```

### References

To build up the `ReferenceMap` we define `ReferenceCount`.

``` {.haskell #parse-markdown}
type ReferencePair = (ReferenceId, CodeBlock)

type ReferenceCount = Map ReferenceName Int

countReference :: ( MonadState ReferenceCount m )
               => ReferenceName -> m Int
countReference r = do
    x <- gets (M.findWithDefault 0 r)
    modify (M.insert r (x + 1))
    return x

newReference :: ( MonadState ReferenceCount m )
             => ReferenceName -> m ReferenceId
newReference n = ReferenceId n <$> countReference n
```

### Parsing the document

Using these two parsers, we can create a larger parser that works on a line-by-line basis. We define several helpers to create a parser for `ListStream Text` using single line parsers for `Text`.

``` {.haskell #parse-markdown}
type LineParser = Parsec Void Text
type DocumentParser = ReaderT Config (StateT ReferenceCount (Parsec Void (ListStream Text)))

parseLine :: LineParser a -> Text -> Maybe (a, Text)
parseLine p t = either (const Nothing) (\x -> Just (x, t))
              $ parse p "" t

parseLineNot :: (Monoid a) => LineParser a -> Text -> Maybe Text
parseLineNot p t = either (const $ Just t) (const Nothing)
                 $ parse p "" t

tokenLine :: ( MonadParsec e (ListStream Text) m )
          => (Text -> Maybe a) -> m a
tokenLine f = token f mempty
```

To parse markdown, we first try to parse a code block (as given above), stored in `CodeBlock`. If that fails lines are interpreted as other markdown, and stored in `PlainText`.

``` {.haskell #parse-markdown}
codeBlock :: ( MonadParsec e (ListStream Text) m
             , MonadReader Config m 
             , MonadState ReferenceCount m )
          => m ([Content], [ReferencePair])
codeBlock = do
    (props, begin) <- tokenLine (parseLine codeHeader)
    code           <- unlines' 
                   <$> manyTill (anySingle <?> "code line")
                                (try $ lookAhead $ tokenLine (parseLine codeFooter))
    (_, end)       <- tokenLine (parseLine codeFooter)
    language       <- getLanguage props
    ref'           <- getReference props
    return $ case ref' of
        Nothing  -> ( [ PlainText $ unlines' [begin, code, end] ], [] )
        Just ref -> ( [ PlainText begin, Reference ref, PlainText end ]
                    , [ ( ref, CodeBlock language props code ) ] )

normalText :: ( MonadParsec e (ListStream Text) m )
           => m ([Content], [ReferencePair])
normalText = do
    text <- unlines' <$> some (tokenLine (parseLineNot codeHeader))
    return ( [ PlainText text ], [] )

markdown :: DocumentParser ([Content], [ReferencePair])
markdown = mconcat <$> many (codeBlock <|> normalText)

parseMarkdown :: ( MonadReader Config m )
              => FilePath -> Text -> m (Either EntangledError Document)
parseMarkdown f t = do
    cfg <- ask
    let result' = parse (evalStateT (runReaderT markdown cfg) mempty) f (ListStream $ T.lines t)
    return $ case result' of
        Left err              -> Left (TangleError $ T.pack $ show err)
        Right (content, refs) -> Right $
            Document (M.fromList refs)
                     content
                     (getFileMap refs)
```

## Generating output files


### Indentation
Tangling is indentation sensitive. Given two code blocks

~~~markdown
 ``` {.python #multiply}
 x *= i
 ```

 ``` {.python #factorial}
 x = 1
 for i in range(n):
    <<multiply>>
 ```
~~~

We should get the code

``` {.python}
x = 1
for i in range(n):
    x *= i
```

Indentation is done by `lines` $\to$ `map (append indent)` $\to$ `unlines`, with the distinction that empty lines are not indented, and that the inverse of `lines` is `unlines'`, which doesn't append a final newline.

``` {.haskell #generate-code}
indent :: Text -> Text -> Text
indent pre text
    = unlines' 
    $ map indentLine
    $ T.lines text
    where indentLine line
        | line == "" = line
        | otherwise  = T.append (T.pack indent) line
```

``` {.haskell #generate-code}
data CodeLine = PlainCode Text
              | NowebReference ReferenceName Text
              deriving (Eq, Show)

type CodeParser = Parsec Void Text
```

We try to parse every line in a code block as a *noweb* reference. If that fails, the line is accepted as normal source text. A *noweb* reference should stand alone on a line, maybe indented with white space. Space at the end of the line is ignored.

    +--- indentation ---+--- reference  ----+--- possible space ---+
                        <<no-spaces-alowed>>

The function `parseCode` takes the `Text` from a code block and generates a list of `CodeLine`, being either `PlainCode` or `NowebReference`.

``` {.haskell #generate-code}
nowebReference :: CodeParser CodeLine
nowebReference = do
    indent <- space
    string "<<"
    id <- takeWhile1P Nothing (`notElem` " \t<>")
    string ">>"
    space >> eof
    return $ NowebReference (ReferenceName id) indent

parseCode :: ReferenceName -> Text -> [CodeLine]
parseCode name = map parseLine . T.lines
    where parseLine l = either (const $ PlainCode l) id
                      $ parse nowebReference
                              (T.unpack $ unReferenceName name)
                              l
```

## Code expansion

We don't want code blocks refering to themselves. I used to keep a history of visited `ReferenceId`s to prevent circular dependencies. This part will be moved to a validation stage.
I now use a lazy map to provide the recursion, which becomes cumbersome if we also have to detect cycles in the dependency graph.

``` {.haskell #generate-code}
type Dependencies = [ReferenceName]
type ExpandedCode = LM.Map ReferenceName Text
type Annotator = Document -> ReferenceId -> Text
```

We have two types of annotators:

* Naked annotator: creates the output code without annotation. From such an expansion it is not possible to untangle.

* Commenting annotator: adds annotations in comments, from which we can locate the original code block.

``` {.haskell #generate-code}
expandedCode :: Annotator -> Document -> ExpandedCode
expandedCode annotate doc = result
    where result = LM.fromSet (referenceNames doc) expand
          expand name = unlines'
                        $ map (expandCodeSource result name)
                        $ map (annotate doc)
                        $ referencesByName doc name

expandCodeSource :: ExpandedCode -> ReferenceName -> Text -> Text
expandCodeSource result name t = codeLinesToText result $ parseCode name t
```

``` {.haskell #generate-code}
codeLineToText :: ExpandedCode -> CodeLine -> Text
codeLineToText _      (PlainCode x) = Right x
codeLineToText result (NowebReference name i) = indent i <$> result LM.! name
```

Then multiple lines is just concatenating that.

``` {.haskell #generate-code}
codeLinesToText :: ExpandedCode -> [CodeLine] -> Text
codeLinesToText result code = unlines' $ map (codeLineToText result) code
```

``` {.haskell #generate-code}
annotateNaked :: Document -> ReferenceId -> Either EntangledError Text
annotateNaked doc ref = Right $ codeSource $ references doc M.! ref
```

``` {.haskell #generate-code}
comment :: ProgrammingLanguage
        -> Text
        -> m (Either EntangledError Text)
comment (UnknownClass cls) _ = Left $ UnknownLanguageClass cls
comment NoLanguage         _ = Left $ MissingLanguageClass
comment (KnownLanguage lang) text = Right $ pre <> text <> post
    where pre  = languageStartComment lang <> "###"
          post = "###" <> languageCloseComment lang

annotateComment :: Document -> ReferenceId -> Either EntangledError Text
annotateComment doc ref = do
    let code = references doc M.! ref
    pre <- comment (codeLanguage code)
           $ " begin <<" <> (referenceName ref) <> ">>["
           <> T.pack (show $ referenceCount ref) <> "] "
    post <- comment (codeLanguage code) " end "
    return $ unlines' [pre, (codeSource code), post]
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
    , ReferenceName(..)
    , ProgrammingLanguage(..)
    , CodeBlock(..) )
import Config (Config, defaultConfig)
import ListStream

import Text.Megaparsec (parse, Parsec)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)

type Parser = Parsec Void Text

p :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
p x t = parse x "" t

type ParserC = StateT ReferenceCount (ReaderT Config (Parsec Void (ListStream Text)))

pc :: ParserC a -> ListStream Text -> Either (ParseErrorBundle (ListStream Text) Void) a
pc x t = parse (runReaderT (evalStateT x mempty) defaultConfig) "" t

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
                  , Reference $ ReferenceId (ReferenceName "hello-world") 0
                  , PlainText "```" ]
                , [ ( ReferenceId (ReferenceName "hello-world") 0
                    , CodeBlock (KnownLanguage "Python")
                                [CodeClass "python", CodeId "hello-world"]
                                "print(\"Hello, World!\")"
                    ) ]
                )
```

