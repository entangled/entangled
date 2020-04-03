# Configuration

The configuration is written in Dhall and has the following schema:

``` {.dhall #config-schema}
let Comment : Type =
    { start : Text
    , end : Optional Text }

let Language : Type =
    { name : Text
    , identifiers : List Text
    , comment : Comment
    , jupyter : Optional Text }

let Config : Type =
    { languages : List Language
    , watchList : Optional (List Text)
    , database  : Optional Text }
```

## Reading config

``` {.haskell #config-import}
import Dhall (Generic, FromDhall, ToDhall, input, auto, Decoder, union, record, field, list, strictText, setFromDistinctList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
```

We need to match the Dhall schema with types in Haskell

``` {.haskell #config-dhall-schema}
data ConfigComment
    = Line  Text
    | Block { start :: Text, end :: Text }
    deriving (Generic, Show)

instance FromDhall ConfigComment
instance ToDhall ConfigComment

data ConfigLanguage = ConfigLanguage
    { languageName :: Text
    , languageIdentifiers :: [Text]
    , languageComment :: ConfigComment
    , languageJupyter :: Maybe Text
    } deriving (Show)

configLanguage :: Decoder ConfigLanguage
configLanguage = record
    ( ConfigLanguage <$> field "name"        auto
                     <*> field "identifiers" auto
                     <*> field "comment"     auto
                     <*> field "jupyter"     auto
    )

instance Eq ConfigLanguage where
    a == b = (languageName a) == (languageName b)

instance Ord ConfigLanguage where
    compare a b = compare (languageName a) (languageName b)

data Config = Config
    { configLanguages :: Set ConfigLanguage
    , configWatchList :: Maybe [Text]
    , configDatabase  :: Maybe Text
    } deriving (Show)

config :: Decoder Config
config = record
    ( Config <$> field "languages" (setFromDistinctList configLanguage)
             <*> field "watchList" auto
             <*> field "database" auto
    )
```

``` {.haskell file=src/Config.hs}
module Config where

<<config-import>>

import Errors

import qualified Data.Text.IO as T.IO
import TextUtil
<<import-set>>
-- import qualified Toml
-- import Toml (TomlCodec, (.=))

import Data.Function (on)
import Data.List (find, scanl1)
import Control.Applicative ((<|>))
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class
import Control.Monad.Catch
import System.FilePath.Glob (glob)
import System.Directory 
import System.FilePath

<<config-dhall-schema>>
<<config-monoid>>
<<config-defaults>>
<<config-input>>
<<config-reader>>

getDatabasePath :: (MonadIO m, MonadThrow m) => Config -> m FilePath
getDatabasePath cfg = do
    dbPath <- case configDatabase cfg of
        Nothing -> throwM $ SystemError "database not configured"
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    return dbPath

getInputFiles :: (MonadIO m) => Config -> m [FilePath]
getInputFiles cfg = liftIO $ maybe mempty
        (concatMapM (glob . T.unpack))
        (configWatchList cfg)
```

Configuration can be stored in `${XDG_CONFIG_HOME}/entangled/config.dhall`. Also the local directory or its parents may contain a `.entangled.dhall` file. These override settings in the global configuration.

There are currently no customisation options for entangled, but I keep my options open: namespaces for references, enabling future features like git support, you name it. The one component that we do need to configure is adding languages to the mix. Entangled has to know how to generate comments in every language.

## Config monoid

We need to be able to stack configurations, so we implement `Monoid` on `Config`. There is a generic way of doing this using `GHC.Generic`, `DeriveGeneric` language extension and `Generic.Data` module. For the moment this would be a bit overkill.

> Tip from Merijn: put the maybes in a `First`/`Last`/`Alt` instance. This will enforce `<|>` behaviour on the monoid being used.

``` {.haskell #config-monoid}
instance Semigroup Config where
    a <> b = Config (configLanguages a <> configLanguages b)
                    (configWatchList a <> configWatchList b)
                    (configDatabase a <|> configDatabase b)

instance Monoid Config where
    mempty = Config mempty mempty mempty
```

You can see this will grow out of hand as the configuration becomes a bigger thing. Note that the stacking prioritises the left most element. The complete config is then:

``` {.haskell #config-monoid}
configStack :: IO Config
configStack = do
    localConfig <- readLocalConfig
    globalConfig <- readGlobalConfig
    return $ localConfig <> globalConfig <> defaultConfig
```

## Defaults

A somewhat biased list of languages: I don't know half of you half as well as I should like; and I like less than half of you half as well as you deserve.

``` {.haskell #config-defaults}
hashComment         = Line  "#"
lispStyleComment    = Line  ";"
cStyleComment       = Block "/*" "*/"
cppStyleComment     = Line  "//"
haskellStyleComment = Line  "--"
mlStyleComment      = Block "(*" "*)"
xmlStyleComment     = Block "<!--" "-->"
texStyleComment     = Line  "%"

defaultLanguages :: Set ConfigLanguage
defaultLanguages = S.fromList
    [ ConfigLanguage "Awk"         ["awk"]                      hashComment         Nothing 
    , ConfigLanguage "C"           ["c"]                        cStyleComment       Nothing
    , ConfigLanguage "C++"         ["cpp", "c++"]               cppStyleComment     Nothing 
    , ConfigLanguage "CSS"         ["css"]                      cStyleComment       Nothing
    , ConfigLanguage "Elm"         ["elm"]                      haskellStyleComment Nothing
    , ConfigLanguage "Gnuplot"     ["gnuplot"]                  hashComment         Nothing 
    , ConfigLanguage "Haskell"     ["hs", "haskell"]            haskellStyleComment Nothing
    , ConfigLanguage "HTML"        ["html"]                     xmlStyleComment     Nothing
    , ConfigLanguage "LaTeX"       ["latex"]                    texStyleComment     Nothing
    , ConfigLanguage "Lua"         ["lua"]                      haskellStyleComment Nothing
    , ConfigLanguage "OCaml"       ["ocaml"]                    mlStyleComment      Nothing
    , ConfigLanguage "OpenCL"      ["opencl"]                   cStyleComment       Nothing
    , ConfigLanguage "Python"      ["py", "python", "python3"]  hashComment         Nothing
    , ConfigLanguage "Julia"       ["jl", "julia"]              hashComment         Nothing 
    , ConfigLanguage "JavaScript"  ["js", "javascript", "ecma"] cStyleComment       Nothing
    , ConfigLanguage "Make"        ["make", "makefile"]         hashComment         Nothing
    , ConfigLanguage "R"           ["r"]                        hashComment         Nothing
    , ConfigLanguage "Rust"        ["rust"]                     cppStyleComment     Nothing
    , ConfigLanguage "Scheme"      ["scm", "scheme"]            lispStyleComment    Nothing
    , ConfigLanguage "SQLite"      ["sqlite"]                   haskellStyleComment Nothing
    , ConfigLanguage "YAML"        ["yaml"]                     hashComment         Nothing
    ]

defaultConfig :: Config
defaultConfig = Config
    { configDatabase = Just ".entangled/db.sqlite"
    , configWatchList = Nothing
    , configLanguages = defaultLanguages
    }
```

### Reading config files

:::TODO
NYI
:::

``` {.haskell #config-input}
findFileAscending :: String -> IO (Maybe FilePath)
findFileAscending filename = do
    path <- dropTrailingPathSeparator <$> getCurrentDirectory
    let parents = reverse $ scanl1 (</>) $ splitDirectories path
    findFile parents filename

readLocalConfig :: IO Config
readLocalConfig = do
    cfg_path <- maybe (throwM $ SystemError "no config found.") id
             <$> findFileAscending "entangled.dhall"
    input config (T.pack cfg_path)

readGlobalConfig :: IO Config
readGlobalConfig = mempty
```

## Processing

``` {.haskell #config-reader}
lookupLanguage :: Config -> Text -> Maybe ConfigLanguage
lookupLanguage cfg x
    = find (elem x . languageIdentifiers) 
    $ configLanguages cfg

languageFromName :: Config -> Text -> Maybe ConfigLanguage
languageFromName cfg x
    = find ((== x) . languageName)
    $ configLanguages cfg
```

