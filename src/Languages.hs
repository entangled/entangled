module Languages
    ( Language(..)
    ) where

{-| What we should know about a language:
      - it's name
      - what abbreviations may be used to indicate it
      - how to insert single line comments
 -}
data Language = Language
    { languageName          :: String
    , languageAbbreviations :: [String]
    , languageLineComment   :: String
    } deriving (Show, Eq)
