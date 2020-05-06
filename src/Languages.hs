module Languages
    ( Language(..)
    , LineDirective(..)
    ) where

import Data.Function(on)
import Data.Text(Text)
import qualified Data.Text as T

{-| What we should know about a language:
      - it's name
      - what abbreviations may be used to indicate it
      - how to insert single line comments
      - how to insert source line directive
 -}
data Language = Language
    { languageName               ::  String
    , languageAbbreviations      :: [String]
    , languageLineComment        ::  String
    , languageCloseComment       ::  String
    , languageLineDirective      ::  LineDirective
    } deriving (Show, Eq)

data LineDirective = LineDirective { applyLineDirective    :: Int -> FilePath -> Text
                                   , checkForLineDirective :: Text -> Bool }

instance Show LineDirective where
  show x = T.unpack $ applyLineDirective x 1 "src/filename"

instance Eq LineDirective where
  (==) = (==) `on` show
