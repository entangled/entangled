# Text utilities

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
import Test.QuickCheck.Instances.Text ()

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

