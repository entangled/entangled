module Config
    ( Config(..)
    , defaultConfig
    , getCommentString
    , lookupLanguage
    , languageFromName
    ) where

import Languages
import Data.List
import Model

newtype Config = Config
    { configLanguages :: [Language]
    } deriving (Show)

getCommentString :: String -> Config -> Maybe String
getCommentString lang 
    = fmap languageLineComment
    . find ((== lang) . languageName)
    . configLanguages

lookupLanguage :: String -> Config -> Maybe Language
lookupLanguage x cfg
    = find (elem x . languageAbbreviations) 
    $ configLanguages cfg

languageFromName :: String -> Config -> Maybe Language
languageFromName x cfg
    = find ((== x) . languageName)
    $ configLanguages cfg

{-| List of languages. This should eventually end up in a separate
    configuration file.
 -}
defaultLanguages =
    [ Language "C++"         ["cpp", "c++"]               "//"
    , Language "C"           ["c"]                        "//"
    , Language "Rust"        ["rust"]                     "//"
    , Language "Haskell"     ["hs", "haskell"]            "--"
    , Language "Python"      ["py", "python", "python3"]  "#"
    , Language "JavaScript"  ["js", "javascript", "ecma"] "//"
    , Language "Scheme"      ["scm", "scheme"]            ";"
    , Language "R"           ["r"]                        "#"
    ]

defaultConfig = Config defaultLanguages
