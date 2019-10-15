-- ------ language="Haskell" file="app/Config.hs"
module Config where

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ begin <<import-set>>[0]
import qualified Data.Set as S
import Data.Set (Set)
-- ------ end
import qualified Toml

-- ------ begin <<config-types>>[0]
data Entangled = Entangled
    { useNamespaces :: Bool
    } deriving (Show)

data Language = Language
    { languageName :: Text
    , languageIdentifiers :: [Text]
    , languageStartComment :: Text
    , languageCloseComment :: Maybe Text
    } deriving (Show)

instance Ord Language where
    compare = compare . languageName

data Config = Config
    { configEntangled :: Maybe Entangled
    , configLanguages :: Set Language
    } deriving (Show)
-- ------ end
-- ------ begin <<config-tomland>>[0]
entangledCodec :: TomlCodec Entangled
entangledCodec = Entangled
    <$> Toml.bool "use-namespaces" .= useNamespaces

languageCodec :: TomlCodec Language
languageCodec = Language
    <$> Toml.text "name" .= languageName
    <*> Toml.arrayOf Toml._Text "identifiers" .= languageIdentifiers
    <*> Toml.text "start-comment" .= languageStartComment
    <*> Toml.dioptional (Toml.text "close-comment") .= languageCloseComment

configCodec :: TomlCodec Config
    <$> Toml.dioptional (Toml.table entangledCodec "entangled") .= configEntangled
    <*> Toml.set languageCodec "languages" .= configLanguages
-- ------ end
-- ------ begin <<config-monoid>>[0]
instance Semigroup Entangled where
    a <> b = a

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
defaultLanguages = Set.fromList
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
    Just $ Entangled { useNamespaces=False
                     }
    defaultLanguages
-- ------ end

-- ------ end
