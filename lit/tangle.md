# Tangling

``` {.haskell file=src/Tangle.hs}
module Tangle where

<<import-text>>
<<import-map>>

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
            (KnownLanguage . languageName)
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

### Preventing loops
We don't want code blocks refering to themselves.

``` {.haskell #generate-code}
data Source = SourceText Text
            | NowebReference ReferenceName Text
            deriving (Eq, Show)

type CodeParser = Parsec Void Text

type History = [ReferenceId]
type Expander = History -> ReferenceId -> Either EntangledError Text
```

We try to parse every line in a code block as a *noweb* reference. If that fails, the line is accepted as normal source text. A *noweb* reference should stand alone on a line, maybe indented with white space. Space at the end of the line is ignored.

    +--- indentation ---+--- reference  ----+--- possible space ---+
                        <<no-spaces-alowed>>

``` {.haskell #generate-code}
nowebReference :: CodeParser Source
nowebReference = do
    indent <- space
    string "<<"
    id <- takeWhile1P Nothing (`notElem` " \t<>")
    string ">>"
    space >> eof
    return $ NowebReference (ReferenceName id) indent

parseCode :: Text -> [Source]
parseCode = map parseLine . T.lines
    where parseLine l = either (const $ SourceText l) id
                      $ parse nowebReference "" l
```

``` {.haskell #generate-code}
codeLineToString :: ReferenceMap 
                 -> Expander 
                 -> History
                 -> Source
                 -> Either EntangledError Text
codeLineToString _ _ _ (SourceText x) = Right $ T.pack x
codeLineToString refs e h (NowebReference r i) = 
    addIndent i . T.concat <$> mapM (e h) (allNameReferences r refs)

codeListToString :: ReferenceMap -> Expander -> History -> [Source] -> Either TangleError T.Text
codeListToString refs e h srcs = T.unlines <$> mapM (codeLineToString refs e h) srcs

expandGeneric :: Expander -> (CodeBlock -> Either TangleError T.Text) -> ReferenceMap -> History -> ReferenceId -> Either TangleError T.Text
expandGeneric e codeToText refs h id = do
    when (id `elem` h) $ Left $ CyclicReference $ show id ++ " in " ++ show h
    codeBlock <- maybeToEither 
        (TangleError $ "Reference " ++ show id ++ " not found.")
        (refs Map.!? id)
    text <- codeToText codeBlock
    parsedCode <- parseCode id $ T.unpack text ++ "\n"
    codeListToString refs e (id : h) parsedCode

expandNaked :: ReferenceMap -> History -> ReferenceId -> Either TangleError T.Text
expandNaked refs = expandGeneric (expandNaked refs) (Right . codeSource) refs

tangleNaked :: Document -> FileMap
tangleNaked (Document refs _) = Map.fromList $ zip fileNames sources
    where fileRefs  = filter isFileReference (Map.keys refs)
          sources   = map (expandNaked refs []) fileRefs
          fileNames = map referenceName fileRefs

{- Annotated tangle -}
topAnnotation :: ReferenceId -> String -> String -> String -> String
topAnnotation (NameReferenceId n i) comment close lang =
    comment ++ " begin <<" ++ n ++ ">>[" ++ show i ++ "]" ++ close
topAnnotation (FileReferenceId n) comment close lang =
    comment ++ " language=\"" ++ lang ++ "\" file=\"" ++ n ++ "\"" ++ close

lookupLanguage' :: (MonadReader Config m)
        => String -> m (Either TangleError Language)
lookupLanguage' name = do
    lang <- reader $ languageFromName name
    case lang of
        Nothing -> return $ Left $ TangleError $ "unknown language: " ++ name
        Just l  -> return $ Right l

annotate :: (MonadReader Config m)
        => ReferenceId -> CodeBlock -> m (Either TangleError T.Text)
annotate id (CodeBlock lang _ text) = do
    l <- lookupLanguage' lang
    return $ do 
        (Language _ _ comment close) <- l
        let top      = T.pack $ topAnnotation id comment close lang
            bottom   = T.pack $ comment ++ " end" ++ close
        return $ top <> T.pack "\n" <> text <> T.pack "\n" <> bottom

expandAnnotated :: (MonadReader Config m)
        => ReferenceMap -> History -> ReferenceId -> m (Either TangleError T.Text)
expandAnnotated refs h id = do
    cfg <- ask
    return $ expandGeneric (\h' id' -> runReader (expandAnnotated refs h' id') cfg)
                           (\cb -> runReader (annotate id cb) cfg) refs h id

tangleAnnotated :: (MonadReader Config m) => ReferenceMap -> m FileMap
tangleAnnotated refs = do
    let fileRefs  = filter isFileReference (Map.keys refs)
        fileNames = map referenceName fileRefs
    sources <- mapM (expandAnnotated refs []) fileRefs
    return $ Map.fromList $ zip fileNames sources
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

