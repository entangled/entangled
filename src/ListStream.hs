-- ------ language="Haskell" file="src/ListStream.hs"
module ListStream where

import Text.Megaparsec (Stream (..), PosState (..), SourcePos (..), mkPos, unPos)
import Data.Proxy (Proxy (..))

-- ------ begin <<instance-list-stream>>[0]
newtype ListStream a = ListStream { unListStream :: [a] }
    deriving (Foldable, Semigroup, Monoid, Functor, Applicative, Monad)

instance (Eq a, Ord a, Show a) => Stream (ListStream a) where
    -- ------ begin <<list-stream-associated-types>>[0]
    type Token (ListStream a) = a
    type Tokens (ListStream a) = [a]
    -- ------ end
    -- ------ begin <<list-stream-methods>>[0]
    tokenToChunk Proxy = pure
    -- ------ end
    -- ------ begin <<list-stream-methods>>[1]
    tokensToChunk Proxy = id
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    -- ------ end
    -- ------ begin <<list-stream-methods>>[2]
    take1_ (ListStream [])     = Nothing
    take1_ (ListStream (x:xs)) = Just (x, ListStream xs)
    takeN_ n (ListStream xs)
        | n <= 0    = Just ([], ListStream xs)
        | null xs   = Nothing
        | otherwise = Just (h, ListStream t) where (h, t) = splitAt n xs
    takeWhile_ p (ListStream xs) = (t, ListStream h) where (h, t) = break p xs
    -- ------ end
    -- ------ begin <<list-stream-methods>>[3]
    showTokens Proxy = show
    -- ------ end
    -- ------ begin <<list-stream-methods>>[4]
    reachOffset offset state@PosState{..} = (sourcePos, repr, state')
        where sourcePos = offsetSourcePos (offset - pstateOffset) pstateSourcePos
              input     = ListStream $ drop (offset - pstateOffset) (unListStream pstateInput)
              repr      = if null input then "<end of stream>" else show (head $ unListStream input)
              state'    = state
                        { pstateInput = input
                        , pstateOffset = offset
                        , pstateSourcePos = sourcePos }
    -- ------ end
-- ------ end

-- ------ begin <<list-stream-helpers>>[0]
offsetSourcePos offset sourcePos@SourcePos{..}
    = sourcePos { sourceLine = mkPos (unPos sourceLine + offset) }
-- ------ end
-- ------ end
