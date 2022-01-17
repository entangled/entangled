# Untangling

```{.haskell file=src/Stitch.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Stitch where

import RIO hiding (some)
import qualified RIO.Text as T
import RIO.List.Partial (head, tail)

<<stitch-imports>>
<<source-parser>>
<<stitch>>
```

Untangling starts with reading the top line to identify the file and language. Following that should be one series of referenced code block items.

The result is a list of `ReferencePair` giving all the content of the code blocks as it is given in the source file. We don't yet make it a `ReferenceMap`, since there may be duplicate conflicting entries. In this case we want to get the entry that is different from the one that we already know of.

``` {.haskell #source-parser}
sourceDocument :: ( MonadParsec e (ListStream Text) m
                  , MonadFail m
                  , MonadReader Config m )
               => m [ReferencePair]
sourceDocument = do
    cfg <- ask
    (header, (prop, _)) <- manyTill_ anySingle (tokenP topHeader)
    -- (prop, _) <- tokenP topHeader
    lang <- maybe (fail "No valid language found in header.") return
                  $ getAttribute prop "language" >>= languageFromName cfg
    (_, refs) <- mconcat <$> some (sourceBlock lang)
    return (prependCode header (head refs) : tail refs)
    where prependCode h (ref, cb@CodeBlock{..})
            = (ref, cb {codeSource = unlines' (h <> [codeSource])})
```

A `sourceBlock` starts with a *begin* marker, then has many lines of plain source or nested `sourceBlock`s. Both `sourceBlock` and `sourceLine` return pairs of texts and references. The content of these pairs are concatenated. If a `sourceBlock` is the first in a series (index 0), the noweb reference is generated with the correct indentation.

``` {.haskell #source-parser}
sourceBlock :: ( MonadReader Config m, MonadParsec e (ListStream Text) m, MonadFail m )
            => ConfigLanguage -> m ([Text], [ReferencePair])
sourceBlock lang = do
    Config{..} <- ask
    ((ref, beginIndent), _) <- tokenP (commented lang beginBlock)
    when configUseLineDirectives (void anySingle)
    (ilines, refpairs) <- mconcat <$> manyTill
                (sourceBlock lang <|> sourceLine)
                (tokenP (commented lang endBlock))
    let unindentedLines = map (unindent beginIndent) ilines
    when (any isNothing unindentedLines) $ fail "Indentation error"
    let content = unlines' $ catMaybes unindentedLines
    return ( [ indent beginIndent $ showNowebReference $ referenceName ref
             | referenceCount ref == 0 ]
           , (ref, CodeBlock (KnownLanguage $ languageName lang) [] content Nothing):refpairs )

sourceLine :: ( MonadParsec e (ListStream Text) m )
           => m ([Text], [ReferencePair])
sourceLine = do
    x <- anySingle
    return ([x], [])
```

## The `stitch` function

In the `stitch` function we take out the `Config` from the anonymous `MonadReader` and put it in a `SourceParser` monad. This transformation is the `asks . runReaderT` combo. It seems silly that we can't "inherit" the outer monad here. I tried turning the transformers around like: `ParsecT Void (ListStream Text) m`, but type deduction fails on that one.

``` {.haskell #stitch}
type SourceParser = ReaderT Config (Parsec Void (ListStream Text))

untangle :: ( MonadReader env m, HasConfig env, MonadThrow m )
         => FilePath -> Text -> m [ReferencePair]
untangle file text = do
    doc <- runReaderT (sourceDocument :: SourceParser [ReferencePair]) <$> view config
    either (throwM . StitchError . T.pack . errorBundlePretty)
           return $ parse doc file $ ListStream (T.lines text)
```

## Imports

``` {.haskell #stitch-imports}
import ListStream (ListStream(..), tokenP)
import Document
import Config (config, HasConfig, Config(..), languageFromName, ConfigLanguage(..))
import Comment (topHeader, beginBlock, endBlock, commented)
import TextUtil (indent, unindent, unlines')

import Text.Megaparsec
    ( MonadParsec, Parsec, parse, anySingle, manyTill, some, errorBundlePretty, manyTill_ )
```
