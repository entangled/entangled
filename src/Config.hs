module Config
    ( Config(..)
    , defaultConfig
    , getCommentString
    , lookupLanguage
    , languageFromName
    ) where

import Languages
import Data.List

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
defaultLanguages :: [ Language ]
defaultLanguages =
    [ Language "C++"         ["cpp", "c++"]               "// ------" ""
    , Language "C"           ["c"]                        "// ------" ""
    , Language "Rust"        ["rust"]                     "// ------" ""
    , Language "Haskell"     ["hs", "haskell"]            "-- ------" ""
    , Language "Python"      ["py", "python", "python3"]  "## ------" ""
    , Language "Julia"       ["jl", "julia"]              "## ------" ""
    , Language "JavaScript"  ["js", "javascript", "ecma"] "// ------" ""
    , Language "Scheme"      ["scm", "scheme"]            ";; ------" ""
    , Language "R"           ["r"]                        "## ------" ""
    , Language "YAML"        ["yaml"]                     "## ------" ""
    , Language "Gnuplot"     ["gnuplot"]                  "## ------" ""
    , Language "Make"        ["make", "makefile"]         "## ------" ""
    , Language "Elm"         ["elm"]                      "-- ------" ""
    , Language "HTML"        ["html"]                     "<!-- ----" " -->"
    , Language "CSS"         ["css"]                      "/* ------" " */"
    , Language "Awk"         ["awk"]                      "## ------" ""
    , Language "OCaml"       ["ocaml"]                    "(* ------" " *)"
    , Language "LaTeX"       ["latex"]                    "%% ------" ""
    , Language "Lua"         ["lua"]                      "## ------" ""
    ]

defaultConfig :: Config
defaultConfig = Config defaultLanguages
