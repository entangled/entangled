# Tangling

``` {.haskell file=src/Tangle.hs}
module Tangle where

import RIO (view, tshow, Map)
<<import-text>>
<<import-map>>
<<import-lazy-map>>

<<import-megaparsec>>

import Control.Monad.Reader (MonadReader, reader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, gets, modify, StateT, evalStateT)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad ((>=>))

import Data.Maybe (catMaybes)

import ListStream (ListStream (..))
import Document
import Config (config, HasConfig, Config, lookupLanguage, ConfigLanguage(..) )

<<tangle-imports>>

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

## CSS Attributes

We'll reuse the attribute parser later on, so we put it in a separate module.

``` {.haskell #tangle-imports}
import Attributes (attributes)
```

``` {.haskell file=src/Attributes.hs}
module Attributes where

<<attributes-imports>>
<<parse-attributes>>
```

``` {.haskell #attributes-imports}
import Data.Text (Text)
import Document (CodeProperty(..))
import Text.Megaparsec
    ( MonadParsec, takeWhile1P, takeWhileP, chunk, endBy, (<|>) )
import Text.Megaparsec.Char
    ( space )
```

The one function that we will use is the `attributes` parser.

``` {.haskell #parse-attributes}
attributes :: (MonadParsec e Text m)
           => m [CodeProperty]
attributes = (  codeClass
            <|> codeId
            <|> codeAttribute
             ) `endBy` space
```

#### Identifiers and values

Anything goes, as long as it doesn't conflict with a space separated curly braces delimited list.

``` {.haskell #parse-attributes}
cssIdentifier :: (MonadParsec e Text m)
              => m Text
cssIdentifier = takeWhile1P (Just "identifier")
                            (\c -> notElem c (" {}=<>|" :: String))

cssValue :: (MonadParsec e Text m)
         => m Text
cssValue = takeWhileP (Just "value")
                      (\c -> notElem c (" {}=<>|" :: String))
```

#### class
A class starts with a period (`.`), and cannot contain spaces or curly braces.

``` {.haskell #parse-attributes}
codeClass :: (MonadParsec e Text m)
          => m CodeProperty
codeClass = do
    chunk "."
    CodeClass <$> cssIdentifier
```

#### id
An id starts with a hash (`#`), and cannot contain spaces or curly braces.

``` {.haskell #parse-attributes}
codeId :: (MonadParsec e Text m)
       => m CodeProperty
codeId = do
    chunk "#"
    CodeId <$> cssIdentifier
```

#### attribute
An generic attribute is written as key-value-pair, separated by an equals sign (`=`). Again no spaces or curly braces are allowed inside.

``` {.haskell #parse-attributes}
codeAttribute :: (MonadParsec e Text m)
              => m CodeProperty
codeAttribute = do
    key <- cssIdentifier
    chunk "="
    value <- cssValue
    return $ CodeAttribute key value
```

## Quasi-parsing Markdown

Parsing the markdown using MegaParsec,

### Parsing code blocks

::: {.TODO}
Add Wirth Syntax Notation with all the parsers.
:::

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
    props <- attributes
    chunk "}" >> space >> eof
    return props 

codeFooter :: (MonadParsec e Text m)
           => m ()
codeFooter = chunk "```" >> space >> eof
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
            <$> reader (\cfg -> languageName <$> lookupLanguage cfg cls)
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
    where filePair (ref, CodeBlock{..}) = do
              path <- getFilePath $ codeProperties
              case codeLanguage of
                  KnownLanguage l -> return (path, (referenceName ref, l))
                  _               -> Nothing
```

### References

To build up the `ReferenceMap` we define `ReferenceCount`.

``` {.haskell #parse-markdown}
data ReferenceCount = ReferenceCount
    { currentDocument :: FilePath
    , refCounts       :: Map ReferenceName Int }

countReference :: ( MonadState ReferenceCount m )
               => ReferenceName -> m Int
countReference r = do
    x <- gets ((M.findWithDefault 0 r) . refCounts)
    modify $ \s -> s { refCounts = M.insert r (x + 1) (refCounts s) }
    return x

newReference :: ( MonadState ReferenceCount m )
             => ReferenceName -> m ReferenceId
newReference n = do
    doc <- gets currentDocument
    x   <- countReference n
    return $ ReferenceId doc n x
```

### Parsing the document

::: {.TODO}
Add Wirth Syntax Notation with all the parsers.
:::

Using these two parsers, we can create a larger parser that works on a line-by-line basis. We define several helpers to create a parser for `ListStream Text` using single line parsers for `Text`.

``` {.haskell #tangle-imports}
import ListStream (parseLine, parseLineNot, tokenLine)
```

To parse markdown, we first try to parse a code block (as given above), stored in `CodeBlock`. If that fails lines are interpreted as other markdown, and stored in `PlainText`.

``` {.haskell #parse-markdown}
type DocumentParser = ReaderT Config (StateT ReferenceCount (Parsec Void (ListStream Text)))

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

parseMarkdown' :: ( MonadReader env m, HasConfig env, MonadThrow m )
               => FilePath -> Text -> m Document
parseMarkdown' f t = do
    cfg <- view config
    let result' = parse (evalStateT (runReaderT markdown cfg) (ReferenceCount f mempty))
                        f (ListStream $ T.lines t)
    case result' of
        Left err              -> throwM $ TangleError $ tshow err
        Right (content, refs) -> return $ Document (M.fromList refs) content (getFileMap refs)
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

``` {.haskell #tangle-imports}
import TextUtil (indent, unlines')
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
    indent <- takeWhileP Nothing (`elem` (" \t" :: [Char]))
    _ <- chunk "<<"
    id <- takeWhile1P Nothing (`notElem` (" \t<>" :: [Char]))
    _ <- chunk ">>"
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
type ExpandedCode = LM.Map ReferenceName (Either EntangledError Text)
type Annotator = ReferenceMap -> ReferenceId -> (Either EntangledError Text)
```

The map of expanded code blocks is generated using an induction pattern here illustrated on lists. Suppose we already have the resulting `output` and a function `g :: [Output] -> Input -> Output` that generates any element in `output` given an input and the rest of all `output`, then `f` generates `output` as follows.

``` {.haskell}
f :: [Input] -> [Output]
f input = output
    where output = map generate input
          generate = g output
```

Lazy evaluation does the rest.

``` {.haskell #generate-code}
expandedCode :: Annotator -> ReferenceMap -> ExpandedCode
expandedCode annotate refs = result
    where result = LM.fromSet expand (referenceNames refs)
          expand name = unlines' <$> (sequence
                        $ map (annotate refs >=> expandCodeSource result name)
                        $ referencesByName refs name)

expandCodeSource :: ExpandedCode -> ReferenceName -> Text
                 -> Either EntangledError Text
expandCodeSource result name t
    = unlines' <$> mapM codeLineToText (parseCode name t)
    where codeLineToText (PlainCode x) = Right x
          codeLineToText (NowebReference name i)
              = indent i <$> result LM.! name
```

We have two types of annotators:

* Naked annotator: creates the output code without annotation. From such an expansion it is not possible to untangle.

``` {.haskell #generate-code}
annotateNaked :: ReferenceMap -> ReferenceId -> Either EntangledError Text
annotateNaked refs ref = Right $ codeSource $ refs M.! ref

annotateComment' :: Config -> Annotator
annotateComment' cfg rmap rid = runReaderT (annotateComment rmap rid) cfg
```

* Commenting annotator: adds annotations in comments, from which we can locate the original code block.

We put comments in a separate module, where we also address parsing back the generated comments.

``` {.haskell #tangle-imports}
import Comment (annotateComment)
```

## Entangled comments

``` {.haskell file=src/Comment.hs}
module Comment where

<<comment-imports>>

<<generate-comment>>
<<parse-comment>>
```

``` {.haskell #comment-imports}
import Control.Monad.Reader
import Control.Monad.Except

<<import-text>>
```

Tangled files start with a single line header comment, followed by nested blocks of (possibly indented) code delimeted by lines:

``` {.c}
\\ -#- entangled -#- begin <<reference-name>>[3]
code content;
\\ -#- entangled -#- end
```

The `-#- entangled -#-` bit is stored in `delim`.

``` {.haskell #generate-comment}
delim :: Text
delim = " ~\\~ "
```

Given any content, the `comment` function generates a commented line following the above prescription.

``` {.haskell #comment-imports}
import Document
    ( ProgrammingLanguage(..)
    , EntangledError(..) )
import Config
    ( Config(..), ConfigLanguage(..), ConfigComment(..), languageFromName )
```

``` {.haskell #generate-comment}
comment :: (MonadReader Config m, MonadError EntangledError m)
        => ProgrammingLanguage
        -> Text
        -> m Text
comment (UnknownClass cls) _ = throwError $ UnknownLanguageClass cls
comment NoLanguage         _ = throwError $ MissingLanguageClass
comment (KnownLanguage langName) text = do
    cfg <- ask
    maybe (throwError $ SystemError $ "language named " <> langName <> " is not in config.")
          (\lang -> return $ formatComment lang text)
          (languageFromName cfg langName)

commentStart :: ConfigComment -> Text
commentStart (Block x _) = x
commentStart (Line x) = x

commentEnd :: ConfigComment -> Maybe Text
commentEnd (Block _ x) = Just x
commentEnd (Line _) = Nothing

formatComment :: ConfigLanguage -> Text -> Text
formatComment lang text = pre <> text <> post
    where pre  = (commentStart $ languageComment lang) <> delim
          post = maybe "" (" " <>) $ commentEnd $ languageComment lang
```

Using this we can write the `annotateComment` function. Given a `ReferenceId` this retrieves the code text and annotates it with a begin and end comment line.

``` {.haskell #comment-imports}
import qualified Data.Map.Strict as M

import Document
import TextUtil (unlines')
```

``` {.haskell #generate-comment}
annotateComment :: (MonadReader Config m, MonadError EntangledError m)
                => ReferenceMap -> ReferenceId -> m Text
annotateComment refs ref = do
    let code = refs M.! ref
    pre <- comment (codeLanguage code)
           $ "begin <<" <> (T.pack $ referenceFile ref) <> "|"
           <> (unReferenceName $ referenceName ref) <> ">>["
           <> T.pack (show $ referenceCount ref) <> "]"
    post <- comment (codeLanguage code) "end"
    return $ unlines' [pre, (codeSource code), post]

headerComment :: ConfigLanguage -> FilePath -> Text
headerComment lang path = formatComment lang
    $ "language=" <> languageName lang <> " filename=" <> T.pack path
```

### Parsing comments

``` {.haskell #comment-imports}
import Text.Megaparsec
    ( MonadParsec, chunk, skipManyTill, anySingle, (<?>), takeWhileP, eof )
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer
    ( decimal )

import Document (CodeProperty)
import Attributes (attributes, cssIdentifier, cssValue)
```

The same comment lines have to be parsed back when we untangle. The first line is the top header comment. We don't know yet what the language is going to be, so we `skipManyTill` we find the `delim` text.

``` {.haskell #parse-comment}
topHeader :: ( MonadParsec e Text m )
          => m [CodeProperty]
topHeader = do
    skipManyTill (anySingle <?> "open comment")
                 (chunk delim)
    attributes
```

Other parsers will always be combined with `commented`, giving the value of the original parser and a `Text` that gives the indentation level of the parsed comment.

``` {.haskell #parse-comment}
commented :: (MonadParsec e Text m)
          => ConfigLanguage -> m a -> m (a, Text)
commented lang p = do 
    indent <- takeWhileP (Just "initial indent") (`elem` (" \t" :: [Char]))
    pre <- chunk $ (commentStart $ languageComment lang) <> delim
    x <- p
    post <- chunk $ (maybe "" id $ commentEnd $ languageComment lang)
    space
    eof
    return (x, indent)
```

``` {.haskell #parse-comment}
beginBlock :: (MonadParsec e Text m)
           => m ReferenceId
beginBlock = do
    chunk "begin <<"
    doc  <- cssValue
    chunk "|"
    name <- cssIdentifier
    chunk ">>["
    count <- decimal
    chunk "]"
    return $ ReferenceId (T.unpack doc) (ReferenceName name) count

endBlock :: (MonadParsec e Text m)
         => m ()
endBlock = chunk "end" >> return ()
```

## Testing

``` {.haskell file=test/TangleSpec.hs}
module TangleSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

<<import-text>>
import qualified Data.Text.IO as T.IO

import qualified Data.Map.Lazy as LM
import Data.List ((!!))

import Tangle
import Attributes
import Document
import Config (Config, readLocalConfig, languageFromName)
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

emptyrc = ReferenceCount "" mempty

pc :: Config -> ParserC a -> ListStream Text -> Either (ParseErrorBundle (ListStream Text) Void) a
pc cfg x = parse (runReaderT (evalStateT x emptyrc) cfg) ""

pd :: Config -> Text -> Either EntangledError Document
pd cfg t = runReader (parseMarkdown "" t) cfg

tangleSpec :: Config -> Spec
tangleSpec cfg = do
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
            pc cfg codeBlock test1 `shouldParse`
                ( [ PlainText "``` {.python #hello-world}"
                  , Reference $ ReferenceId "" (ReferenceName "hello-world") 0
                  , PlainText "```" ]
                , [ ( ReferenceId "" (ReferenceName "hello-world") 0
                    , CodeBlock (KnownLanguage "Python")
                                [CodeClass "python", CodeId "hello-world"]
                                "print(\"Hello, World!\")"
                    ) ]
                )

tangleEqualSpec :: Config -> Spec
tangleEqualSpec cfg = before (mapM T.IO.readFile testFiles) $ do
    describe "Code expansion on test0[1-3].md" $ do
        it "expands to identical code" $ \files -> do
            let docs = rights $ map (pd cfg) files
                codes = map (expandedCode annotateNaked . references) docs
                hellos = map (LM.! ReferenceName "hello.cc") codes
            hellos `shouldBe` (replicate 3 (hellos !! 0))
    where testFiles = ["test/test01.md", "test/test02.md", "test/test03.md"]
```
