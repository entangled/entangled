{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}
module Config
    ( Config(..)
    , defaultConfig
    , languageFromName
    , languageFromAbbrev
    , disableLineDirectives
    ) where

import Languages
import Data.List
import Model
import qualified Data.Text as T

newtype Config = Config
    { configLanguages :: [Language]
    } deriving (Show)

languageFromAbbrev :: String -> Config -> Maybe Language
languageFromAbbrev x cfg
    = find (elem x . languageAbbreviations) 
    $ configLanguages cfg

languageFromName :: String -> Config -> Maybe Language
languageFromName x cfg
    = find ((== x) . languageName)
    $ configLanguages cfg

{-| List of languages. This should eventually end up in a separate
    configuration file.
 -}
defaultLanguages :: [Language]
defaultLanguages =
    [ Language "C++"         ["cpp", "c++"]               "// ------" ""     cppLineDirective
    , Language "C"           ["c"]                        "// ------" ""     cppLineDirective
    , Language "D"           ["d"]                        "// ------" ""     noLineDirective
    , Language "Rust"        ["rust"]                     "// ------" ""     noLineDirective
    , Language "Haskell"     ["hs", "haskell"]            "-- ------" ""     haskellLineDirective
    , Language "Python"      ["py", "python", "python3"]  "## ------" ""     noLineDirective
    , Language "Julia"       ["jl", "julia"]              "## ------" ""     noLineDirective
    , Language "JavaScript"  ["js", "javascript", "ecma", "json"] "// ------" ""     noLineDirective
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
cppLineDirective = LineDirective {..}
  where
    applyLineDirective lineNo filename = mconcat ["#LINE ", T.pack (show lineNo), " \"", T.pack filename, "\""]
    checkForLineDirective              = T.isPrefixOf "#LINE "

haskellLineDirective :: LineDirective
haskellLineDirective = LineDirective {..}
  where
    applyLineDirective lineNo filename = mconcat ["{-# LINE ", T.pack (show lineNo), " \"", T.pack filename, "\" #-}"]
    checkForLineDirective              = T.isPrefixOf "{-# LINE "

noLineDirective :: LineDirective
noLineDirective = LineDirective (\_ _ -> "") (\_ -> False)

defaultConfig = Config defaultLanguages

disableLineDirectives :: Config -> Config
disableLineDirectives conf =
    conf { configLanguages = disableLineDirective <$> configLanguages conf }
  where
    disableLineDirective lang = lang { languageLineDirective = noLineDirective }
