# Parsing anything with Megaparsec

Megaparsec is considered to be preferable over older Parsec. It is a bit more work to parse different things than strings though. We will be parsing lists of items.

``` {.haskell file=src/ListStream.hs}
module ListStream where

import Data.Text (Text)
import Text.Megaparsec (Parsec, MonadParsec, token, parse)
import Text.Megaparsec (Stream (..), PosState (..), SourcePos (..), mkPos, unPos)
import Data.Proxy (Proxy (..))
import Data.Void (Void)

<<instance-list-stream>>

<<list-stream-helpers>>
<<line-parser>>
```

The minimal definition of a stream must define `tokensToChunk`, `chunkToTokens`, `chunkLength`, `take1_`, `takeN_`, `takeWhile_`, `showTokens` and `reachOffset`.

Because our instance will overlap with that of `[Char] = String` we'll have to wrap the `ListStream` in a `newtype`.

``` {.haskell #instance-list-stream}
newtype ListStream a = ListStream { unListStream :: [a] }
    deriving (Show, Foldable, Semigroup, Monoid, Functor, Applicative, Monad)

instance (Eq a, Ord a, Show a) => Stream (ListStream a) where
    <<list-stream-associated-types>>
    <<list-stream-methods>>
```

``` {.haskell #list-stream-associated-types}
type Token (ListStream a) = a
type Tokens (ListStream a) = [a]
```

Converting a token to a chunk is just `pure`.

``` {.haskell #list-stream-methods}
tokenToChunk Proxy = pure
```

Tokens to chunk or even chunk to tokens are both the identity function

``` {.haskell #list-stream-methods}
tokensToChunk Proxy = id
chunkToTokens Proxy = id
chunkLength Proxy = length
chunkEmpty Proxy = null
```

In fact, these definitions are identical to those in the instance definition for `Stream String` in Megaparsec.

``` {.haskell #list-stream-methods}
take1_ (ListStream [])     = Nothing
take1_ (ListStream (x:xs)) = Just (x, ListStream xs)
takeN_ n (ListStream xs)
    | n <= 0    = Just ([], ListStream xs)
    | null xs   = Nothing
    | otherwise = Just (h, ListStream t) where (h, t) = splitAt n xs
takeWhile_ p (ListStream xs) = (t, ListStream h) where (h, t) = break p xs
```

``` {.haskell #list-stream-methods}
showTokens Proxy = show
```

Here's where things get different. Offsets are lines in a source file. Therefore, `sourceColumn` is always 1. The precise working of `reachOffset` is a bit guesswork.

``` {.haskell #list-stream-helpers}
offsetSourcePos offset sourcePos@SourcePos{..}
    = sourcePos { sourceLine = mkPos (unPos sourceLine + offset) }
```

``` {.haskell #list-stream-methods}
reachOffset offset state@PosState{..} = (sourcePos, repr, state')
    where sourcePos = offsetSourcePos (offset - pstateOffset) pstateSourcePos
          input     = ListStream $ drop (offset - pstateOffset) (unListStream pstateInput)
          repr      = if null input then "<end of stream>" else show (head $ unListStream input)
          state'    = state
                    { pstateInput = input
                    , pstateOffset = offset
                    , pstateSourcePos = sourcePos }
```

## Text-parser tokens

``` {.haskell #line-parser}
type LineParser = Parsec Void Text

parseLine :: LineParser a -> Text -> Maybe (a, Text)
parseLine p t = either (const Nothing) (\x -> Just (x, t))
              $ parse p "" t

parseLineNot :: LineParser a -> Text -> Maybe Text
parseLineNot p t = either (const $ Just t) (const Nothing)
                 $ parse p "" t

tokenLine :: ( MonadParsec e (ListStream Text) m )
          => (Text -> Maybe a) -> m a
tokenLine f = token f mempty

tokenP :: ( MonadParsec e (ListStream Text) m )
       => LineParser a -> m (a, Text)
tokenP = tokenLine . parseLine
```


## Testing on lists

``` {.haskell file=test/ListStreamSpec.hs}
module ListStreamSpec where

import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec
import Data.Void

import ListStream

<<list-stream-props>>

listStreamSpec :: Spec
listStreamSpec =
    <<list-stream-spec>>
```

### Lists of integers

``` {.haskell #list-stream-props}
prop_parser :: (Eq a) => Parsec e s a -> s -> a -> Bool
prop_parser p input expected = success (parse p "" input)
    where success (Left _) = False
          success (Right x) = x == expected

parseAny :: (Show a, Ord a, Eq a) => Parsec Void (ListStream a) [a]
parseAny = takeWhileP Nothing (const True)

prop_tokens :: [Int] -> Bool
prop_tokens xs = prop_parser parseAny (ListStream xs) xs
```

``` {.haskell #list-stream-spec}
describe "Parsing integers" $
    it "takeWhileP yield input" $
        property prop_tokens
```
