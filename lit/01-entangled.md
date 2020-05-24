---
title: Entangled, literate programming Swiss army knife
author: Johan Hidding
---

Entangled makes writing literate programs easier by keeping code blocks in markdown up-to-date with generated source files. By monitoring the tangled source files, any change in the master document or source files is reflected in the other. In practice this means:

* Write well documented code using Markdown.
* Use any programming language you like (or are forced to use).
* Keep debugging and using other IDE features without change.
* Generate a report in PDF or HTML from the same source (see examples on the right).

# Preliminaries

## Modules

Several modules have standard stature but have to be imported qualified due to clashes in the namespace.

### Map

We use strict maps by default.

``` {.haskell #import-map}
import qualified Data.Map.Strict as M
```

``` {.haskell #import-lazy-map}
import qualified Data.Map.Lazy as LM
```

### Set

``` {.haskell #import-set}
import qualified Data.Set as S
```

## Text

We will be using the `Text` module everywhere.

``` {.haskell #import-text}
import RIO (Text)
import qualified RIO.Text as T
```

We need a version of `unlines` that doesn't append a final newline. This also means changing `T.lines` a little.

``` {.haskell #unlines}
lines' :: Text -> [Text]
lines' text
    | text == ""               = [""]
    | "\n" `T.isSuffixOf` text = T.lines text <> [""]
    | otherwise                = T.lines text

unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
```

This way, `unlines'` has nicer properties. No single `Text` in Entangled ends in a newline, meaning the `unlines'` operation is associative:

``` {.haskell}
unlines' [unlines' a, unlines' b] = unlines' (a <> b)
```

With the notable exception of empty lists. The `[Text]` type is a proper monoid, however when we consider `Text` itself in the context of an element of an `unlines`ed text, the zero element for `Text`, being `""` changes meaning. An empty line is something different than the absence of text. To preserve both associativity and make the `unlines . lines = id` property more strict we need to wrap `Text` in a `Maybe`.

``` {.haskell #maybe-unlines}
mLines :: Maybe Text -> [Text]
mLines Nothing = []
mLines (Just text) = lines' text

mUnlines :: [Text] -> Maybe Text
mUnlines [] = Nothing
mUnlines ts = Just $ unlines' ts
```

Now we can test:

``` {.haskell #test-unlines-inverse}
t == mUnlines (mLines t)
```

and

``` {.haskell #test-unlines-associative}
mUnlines (catMaybes [mUnlines a, mUnlines b]) == mUnlines (a <> b)
```

### `TextUtils` module 

``` {.haskell file=src/TextUtil.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module TextUtil where

import RIO
import qualified RIO.Text as T

import Data.Char (isSpace)

<<indent>>
<<unindent>>
<<unlines>>
<<maybe-unlines>>
```

``` {.haskell #indent}
indent :: Text -> Text -> Text
indent pre text
    = unlines' $ map indentLine $ lines' text
    where indentLine line
            | line == "" = line
            | otherwise  = pre <> line
```

``` {.haskell #unindent}
unindent :: Text -> Text -> Maybe Text
unindent prefix s
    = unlines' <$> mapM unindentLine (lines' s)
    where unindentLine t
            | T.all isSpace t = Just ""
            | otherwise       = T.stripPrefix prefix t
```

#### Tests

``` {.haskell file=test/TextUtilSpec.hs}
module TextUtilSpec where

<<import-text>>
import Data.Maybe (catMaybes, isJust)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Text

import TextUtil

propUnlines :: Maybe Text -> Bool
propUnlines t = 
    <<test-unlines-inverse>>

propUnlineLists :: ([Text], [Text]) -> Bool
propUnlineLists (a, b) =
    <<test-unlines-associative>>

genLine :: Gen Text
genLine = T.pack <$> (listOf $ elements ['!'..'~'])

genText :: Gen Text
genText = unlines' <$> listOf genLine

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair x y = do
    i <- x
    j <- y
    return (i, j)

propIndent :: (Text, Text) -> Bool
propIndent (a, b) = unindent a (indent a b) == Just b

propUnindentFail :: (Text, Text) -> Bool
propUnindentFail (a, b) = (isJust $ unindent a b) == a `T.isPrefixOf` b

textUtilSpec :: Spec
textUtilSpec = do
    describe "property check" $ do
        it "mUnlines inverses mLines" $
            property $ propUnlines
        it "mUnlines can be nested (associativity)" $
            property $ propUnlineLists
        it "unindent inverts indent" $
            property $ forAll (genPair genLine genText)  propIndent
        it "unindent fails on wrong indent" $
            property $ forAll (genPair genLine genLine) propIndent
```

### MegaParsec

All parsing will be done through megaparsec.

``` {.haskell #import-megaparsec}
import Text.Megaparsec
    ( MonadParsec, Parsec, parse
    , chunk, many, some, eof
    , manyTill, anySingle, try, lookAhead, takeWhile1P, takeWhileP
    , (<?>) )
import Text.Megaparsec.Char
    ( space )
```

## Errors

``` {.haskell file=src/Errors.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Errors where

import RIO

data EntangledError
    = TangleError Text
    | StitchError Text
    | CyclicReference Text
    | UnknownLanguageClass Text
    | DatabaseError Text
    | SystemError Text
    | MissingLanguageClass
    | UnknownError
    deriving (Show, Ord, Eq, Typeable)

toEntangledError :: (Show e)
                 => (Text -> EntangledError) -> Either e a
                 -> Either EntangledError a
toEntangledError _ (Right x) = Right x
toEntangledError f (Left x) = Left $ f $ tshow x

instance Exception EntangledError

formatError :: EntangledError -> Text
formatError (TangleError t) = "tangling: " <> t
formatError (StitchError t) = "stitching: " <> t
formatError x = tshow x
```

# Design

Entangled is a command-line tool, structured around a SQLite database.

