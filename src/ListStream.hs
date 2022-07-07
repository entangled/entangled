-- ~\~ language=Haskell filename=src/ListStream.hs
-- ~\~ begin <<lit/a3-megaparsec.md|src/ListStream.hs>>[init]
{-# LANGUAGE NoImplicitPrelude #-}
module ListStream where

import RIO
import RIO.List (splitAt, headMaybe)
import Text.Megaparsec ( Parsec, MonadParsec, token, parse, satisfy
                       , Stream (..), VisualStream (..), TraversableStream(..)
                       , PosState (..), SourcePos (..), mkPos, unPos )

-- ~\~ begin <<lit/a3-megaparsec.md|instance-list-stream>>[init]
newtype ListStream a = ListStream { unListStream :: [a] }
    deriving (Show, Foldable, Semigroup, Monoid, Functor, Applicative, Monad)

instance (Eq a, Ord a, Show a) => Stream (ListStream a) where
    -- ~\~ begin <<lit/a3-megaparsec.md|list-stream-associated-types>>[init]
    type Token (ListStream a) = a
    type Tokens (ListStream a) = [a]
    -- ~\~ end
    -- ~\~ begin <<lit/a3-megaparsec.md|list-stream-methods>>[init]
    tokenToChunk Proxy = pure
    -- ~\~ end
    -- ~\~ begin <<lit/a3-megaparsec.md|list-stream-methods>>[1]
    tokensToChunk Proxy = id
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    -- ~\~ end
    -- ~\~ begin <<lit/a3-megaparsec.md|list-stream-methods>>[2]
    take1_ (ListStream [])     = Nothing
    take1_ (ListStream (x:xs)) = Just (x, ListStream xs)
    takeN_ n (ListStream xs)
        | n <= 0    = Just ([], ListStream xs)
        | null xs   = Nothing
        | otherwise = Just (h, ListStream t) where (h, t) = splitAt n xs
    takeWhile_ p (ListStream xs) = (t, ListStream h) where (h, t) = break p xs
    -- ~\~ end

instance (Eq a, Ord a, Show a) => VisualStream (ListStream a) where
    -- ~\~ begin <<lit/a3-megaparsec.md|list-visual-stream-methods>>[init]
    showTokens Proxy = show
    -- ~\~ end

instance (Eq a, Ord a, Show a) => TraversableStream (ListStream a) where
    -- ~\~ begin <<lit/a3-megaparsec.md|list-traversable-stream-methods>>[init]
    reachOffset offset state@PosState{..} = (Just repr, state')
        where sourcePos = offsetSourcePos (offset - pstateOffset) pstateSourcePos
              input     = ListStream $ drop (offset - pstateOffset) (unListStream pstateInput)
              repr      = maybe "<end of stream>" show $ headMaybe $ unListStream input
              state'    = state
                        { pstateInput = input
                        , pstateOffset = offset
                        , pstateSourcePos = sourcePos }
    -- ~\~ end
-- ~\~ end

-- ~\~ begin <<lit/a3-megaparsec.md|list-stream-helpers>>[init]
offsetSourcePos :: Int -> SourcePos -> SourcePos
offsetSourcePos offset sourcePos@SourcePos{..}
    = sourcePos { sourceLine = mkPos (unPos sourceLine + offset) }
-- ~\~ end
-- ~\~ begin <<lit/a3-megaparsec.md|line-parser>>[init]
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

tokenLineNot :: ( MonadParsec e (ListStream Text) m )
             => (Text -> Maybe a) -> m Text
tokenLineNot f = satisfy (isNothing . f)

tokenP :: ( MonadParsec e (ListStream Text) m )
       => LineParser a -> m (a, Text)
tokenP = tokenLine . parseLine
-- ~\~ end
-- ~\~ end
