# Tangling

``` {.haskell file=app/Tangle.hs}
<<import-text>>
import Text.Regex.TDFA
<<import-megaparsec>>

<<code-header-regex>>
<<code-footer-regex>>

<<markdown-data>>
<<parse-markdown>>
```

The task of tangling means:

* Parse the markdown to a `Document`
* Generate annotated source files.

Parsing the markdown is done on a line per line basis. We don't try to parse the markdown itself, rather we try to detect lines that start and end code-blocks.

Remember the golden rule:

> Untangling from a generated source returns the same markdown **to the byte**.

Scanning a document, we trigger on each line matching:

``` {.haskell #code-header-regex}
codeHeaderRe = "^``` *{(.*)} *$"
```

A code block is ended with a line:

``` {.haskell #code-footer-regex}
codeFooterRe = "^```` *$"
```

## Matching lines

We distinguish four types of lines,

``` {.haskell #markdown-data}
data Markdown =
    MarkdownLine Text
    CodeHeader Text [CodeProperty]
    CodeLine Text
    CodeFooter Text
```

Parsing the markdown using MegaParsec,

``` {.haskell #parse-markdown}
type Parser = Parsec Void Text

parseLine :: (MonadReader Config m) -> Text -> m Markdown
parseLine = 

parseCodeHeader :: (MonadReader Config m) -> Text -> m (Maybe Markdown)
parseCodeHeader t = do
    re <- asks codeHeaderRe
    return $ do
        (_, _, _, m) <- t =~~ re
        return $ CodeHeader t (getCodeProperties m)

parseCodeFooter :: (MonadReader Config m) -> Text -> m (Maybe Markdown)
parseCodeFooter t = do
    re <- asks codeFooterRe
    return $ if t =~ re then Just (CodeFooter t) else Nothing

parseMarkdown :: (MonadReader Config m) => Text -> m [Markdown]
parseMarkdown t =
```

