-- ~\~ language=Haskell filename=src/ListStream.hs
-- ~\~ begin <<lit/a3-megaparsec.md|src/ListStream.hs>>[0]
module ListStream where

import Data.Text (Text)
import Text.Megaparsec (Parsec, MonadParsec, token, parse)
import Text.Megaparsec (Stream (..), PosState (..), SourcePos (..), mkPos, unPos)
import Data.Proxy (Proxy (..))
import Data.Void (Void)

-- ~\~ begin <<lit/a3-megaparsec.md|instance-list-stream>>[0]
newtype ListStream a = ListStream { unListStream :: [a] }
    deriving (Show, Foldable, Semigroup, Monoid, Functor, Applicative, Monad)

instance (Eq a, Ord a, Show a) => Stream (ListStream a) where
    -- ~\~ begin <<lit/a3-megaparsec.md|list-stream-associated-types>>[0]
    type Token (ListStream a) = a
    type Tokens (ListStream a) = [a]
    -- ~\~ end
    -- ~\~ begin <<lit/a3-megaparsec.md|list-stream-methods>>[0]
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
    -- ~\~ begin <<lit/a3-megaparsec.md|list-stream-methods>>[3]
    showTokens Proxy = show
    -- ~\~ end
    -- ~\~ begin <<lit/a3-megaparsec.md|list-stream-methods>>[4]
    reachOffset offset state@PosState{..} = (sourcePos, repr, state')
        where sourcePos = offsetSourcePos (offset - pstateOffset) pstateSourcePos
              input     = ListStream $ drop (offset - pstateOffset) (unListStream pstateInput)
              repr      = if null input then "<end of stream>" else show (head $ unListStream input)
              state'    = state
                        { pstateInput = input
                        , pstateOffset = offset
                        , pstateSourcePos = sourcePos }
    -- ~\~ end
-- ~\~ end

-- ~\~ begin <<lit/a3-megaparsec.md|list-stream-helpers>>[0]
offsetSourcePos offset sourcePos@SourcePos{..}
    = sourcePos { sourceLine = mkPos (unPos sourceLine + offset) }
-- ~\~ end
-- ~\~ begin <<lit/a3-megaparsec.md|line-parser>>[0]
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

tokenNotP :: ( MonadParsec e (ListStream Text) m )
       => LineParser a -> m Text
tokenNotP = tokenLine . parseLineNot
-- ~\~ end
-- ~\~ end
