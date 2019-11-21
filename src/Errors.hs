-- ------ language="Haskell" file="src/Errors.hs"
module Errors where

import Control.Exception
import Data.Typeable
import TextUtil

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end

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
-- ------ end
