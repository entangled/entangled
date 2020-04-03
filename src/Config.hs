{-# language OverloadedStrings #-}
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
import qualified Data.Text as T

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
    [ Language "C++"         ["cpp", "c++"]               "// ------" ""     cppLineDirective
    , Language "C"           ["c"]                        "// ------" ""     cppLineDirective
    , Language "D"           ["d"]                        "// ------" ""     noLineDirective
    , Language "Rust"        ["rust"]                     "// ------" ""     noLineDirective
    , Language "Haskell"     ["hs", "haskell"]            "-- ------" ""     haskellLineDirective
    , Language "Python"      ["py", "python", "python3"]  "## ------" ""     noLineDirective
    , Language "Julia"       ["jl", "julia"]              "## ------" ""     noLineDirective
    , Language "JavaScript"  ["js", "javascript", "ecma"] "// ------" ""     noLineDirective
    , Language "TypeScript"  ["ts", "typescript"]         "// ------" ""     noLineDirective
    , Language "Haxe"        ["hx", "haxe"]               "// ------" ""     noLineDirective
    , Language "Clojure"     ["clj", "cljs", "clojure"]   ";; ------" ""     noLineDirective
    , Language "Scheme"      ["scm", "scheme"]            ";; ------" ""     noLineDirective
    , Language "R"           ["r"]                        "## ------" ""     noLineDirective
    , Language "YAML"        ["yaml"]                     "## ------" ""     noLineDirective
    , Language "Gnuplot"     ["gnuplot"]                  "## ------" ""     noLineDirective
    , Language "Make"        ["make", "makefile"]         "## ------" ""     noLineDirective
    , Language "Elm"         ["elm"]                      "-- ------" ""     noLineDirective
    , Language "HTML"        ["html"]                     "<!-- ----" " -->" noLineDirective
    , Language "CSS"         ["css"]                      "/* ------" " */"  noLineDirective
    , Language "Awk"         ["awk"]                      "## ------" ""     noLineDirective
    , Language "OpenCL"      ["opencl"]                   "// ------" ""     cppLineDirective
    , Language "Idris"       ["idris"]                    "-- ------" ""     noLineDirective
    , Language "SQLite"      ["sqlite"]                   "-- ------" ""     noLineDirective
    ]

cppLineDirective :: LineDirective
cppLineDirective = LineDirective $ \lineNo filename -> mconcat ["#LINE ", T.pack (show lineNo), " \"", T.pack filename, "\""]

haskellLineDirective :: LineDirective
haskellLineDirective = LineDirective $ \lineNo filename -> mconcat ["{-# LINE ", T.pack (show lineNo), " \"", T.pack filename, "\" #-}"]

noLineDirective :: LineDirective
noLineDirective = LineDirective $ \_ _ -> ""

defaultConfig = Config defaultLanguages
