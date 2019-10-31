-- ------ language="Haskell" file="src/Config.hs"
module Config where

import Data.Text (Text)
-- ------ begin <<import-set>>[0]
import qualified Data.Set as S
import Data.Set (Set)
-- ------ end
import qualified Toml
import Toml (TomlCodec, (.=))
import Data.Function (on)
import Data.List (find)
import Control.Applicative ((<|>))

-- ------ begin <<config-types>>[0]
data Entangled = Entangled
    { watchList :: Maybe [Text]
    , database :: Maybe Text
    , useNamespaces :: Maybe Bool
    } deriving (Show)

data Language = Language
    { languageName :: Text
    , languageIdentifiers :: [Text]
    , languageStartComment :: Text
    , languageCloseComment :: Maybe Text
    } deriving (Show)

instance Eq Language where
    (==) = (==) `on` languageName

instance Ord Language where
    compare = compare `on` languageName

data Config = Config
    { configEntangled :: Maybe Entangled
    , configLanguages :: Set Language
    } deriving (Show)
-- ------ end
-- ------ begin <<config-tomland>>[0]
entangledCodec :: TomlCodec Entangled
entangledCodec = Entangled
    <$> Toml.dioptional (Toml.arrayOf Toml._Text "watch-list") .= watchList
    <*> Toml.dioptional (Toml.text "database") .= database
    <*> Toml.dioptional (Toml.bool "use-namespaces") .= useNamespaces

languageCodec :: TomlCodec Language
languageCodec = Language
    <$> Toml.text "name" .= languageName
    <*> Toml.arrayOf Toml._Text "identifiers" .= languageIdentifiers
    <*> Toml.text "start-comment" .= languageStartComment
    <*> Toml.dioptional (Toml.text "close-comment") .= languageCloseComment

configCodec :: TomlCodec Config
configCodec = Config
    <$> Toml.dioptional (Toml.table entangledCodec "entangled") .= configEntangled
    <*> Toml.set languageCodec "languages" .= configLanguages
-- ------ end
-- ------ begin <<config-monoid>>[0]
instance Semigroup Entangled where
    a <> b = Entangled (watchList a <> watchList b)
                       (database a <|> database b)
                       (useNamespaces a <|> useNamespaces b)

instance Semigroup Config where
    a <> b = Config (configEntangled a <> configEntangled b)
                    (configLanguages a <> configLanguages b)

instance Monoid Config where
    mempty = Config mempty mempty
-- ------ end
-- ------ begin <<config-monoid>>[1]
configStack :: IO Config
configStack = do
    localConfig <- readLocalConfig
    globalConfig <- readGlobalConfig
    return $ localConfig <> globalConfig <> defaultConfig
-- ------ end
-- ------ begin <<config-defaults>>[0]
defaultLanguages :: Set Language
defaultLanguages = S.fromList
    [ Language "C++"         ["cpp", "c++"]               "// ------" Nothing
    , Language "C"           ["c"]                        "// ------" Nothing
    , Language "Rust"        ["rust"]                     "// ------" Nothing
    , Language "Haskell"     ["hs", "haskell"]            "-- ------" Nothing
    , Language "Python"      ["py", "python", "python3"]  "## ------" Nothing
    , Language "Julia"       ["jl", "julia"]              "## ------" Nothing
    , Language "JavaScript"  ["js", "javascript", "ecma"] "// ------" Nothing
    , Language "Scheme"      ["scm", "scheme"]            ";; ------" Nothing
    , Language "R"           ["r"]                        "## ------" Nothing
    , Language "YAML"        ["yaml"]                     "## ------" Nothing
    , Language "Gnuplot"     ["gnuplot"]                  "## ------" Nothing
    , Language "Make"        ["make", "makefile"]         "## ------" Nothing
    , Language "Elm"         ["elm"]                      "-- ------" Nothing
    , Language "HTML"        ["html"]                     "<!-- ----" (Just " -->")
    , Language "CSS"         ["css"]                      "/* ------" (Just " */")
    , Language "Awk"         ["awk"]                      "## ------" Nothing
    , Language "OCaml"       ["ocaml"]                    "(* ------" (Just " *)")
    , Language "LaTeX"       ["latex"]                    "%% ------" Nothing
    , Language "Lua"         ["lua"]                      "-- ------" Nothing
    , Language "OpenCL"      ["opencl"]                   "// ------" Nothing
    ]

defaultConfig :: Config
defaultConfig = Config
    { configEntangled = Just $
          Entangled { useNamespaces=Just False
                    , database=Just ".entangled.sqlite"
                    , watchList=Nothing
                    }
    , configLanguages = defaultLanguages
    }
-- ------ end
-- ------ begin <<config-input>>[0]
readLocalConfig :: IO Config
readLocalConfig = return mempty

readGlobalConfig :: IO Config
readGlobalConfig = return mempty
-- ------ end
-- ------ begin <<config-reader>>[0]
lookupLanguage :: Config -> Text -> Maybe Language
lookupLanguage cfg x
    = find (elem x . languageIdentifiers) 
    $ configLanguages cfg

languageFromName :: Config -> Text -> Maybe Language
languageFromName cfg x
    = find ((== x) . languageName)
    $ configLanguages cfg
-- ------ end
-- ------ end
