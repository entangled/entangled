module Config
    ( Config(..)
    ) where

import Languages

newtype Config = Config
    { configLanguages :: [Language]
    } deriving (Show)

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
