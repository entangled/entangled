-- ~\~ language=Haskell filename=src/Errors.hs
-- ~\~ begin <<lit/01-entangled.md|src/Errors.hs>>[0]
module Errors where

import Control.Exception
import Data.Typeable
import TextUtil

-- ~\~ begin <<lit/01-entangled.md|import-text>>[0]
import RIO (Text)
import qualified RIO.Text as T
-- ~\~ end

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
-- ~\~ end
