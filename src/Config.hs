-- ------ language="Haskell" file="src/Config.hs" project://lit/04-configuration.md#68
module Config where

-- ------ begin <<config-import>>[0] project://lit/04-configuration.md#25
import Dhall (Generic, FromDhall, ToDhall, input, auto)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
-- ------ end

import Errors

import qualified Data.Text.IO as T.IO
import TextUtil
-- ------ begin <<import-set>>[0] project://lit/01-entangled.md#35
import qualified Data.Set as S
import Data.Set (Set)
-- ------ end
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

-- ------ begin <<config-dhall-schema>>[0] project://lit/04-configuration.md#34
data ConfigComment = ConfigComment
    { commentStart :: Text
    , commentEnd :: Maybe Text
    } deriving (Generic, Show)

data ConfigLanguage = ConfigLanguage
    { languageName :: Text
    , languageIdentifiers :: [Text]
    , languageComment :: ConfigComment
    , languageJupyter :: Maybe Text
    } deriving (Generic, Show)

instance Eq ConfigLanguage where
    a == b = (languageName a) == (languageName b)

instance Ord ConfigLanguage where
    compare a b = compare (languageName a) (languageName b)

data Config = Config
    { configLanguages :: Set ConfigLanguage
    , configWatchList :: Maybe [Text]
    , configDatabase  :: Maybe Text
    } deriving (Generic, Show)

instance FromDhall ConfigComment
instance FromDhall ConfigLanguage
instance FromDhall Config

instance ToDhall ConfigComment
instance ToDhall ConfigLanguage
instance ToDhall Config
-- ------ end
-- ------ begin <<config-monoid>>[0] project://lit/04-configuration.md#121
instance Semigroup Config where
    a <> b = Config (configLanguages a <> configLanguages b)
                    (configWatchList a <> configWatchList b)
                    (configDatabase a <|> configDatabase b)

instance Monoid Config where
    mempty = Config mempty mempty mempty
-- ------ end
-- ------ begin <<config-monoid>>[1] project://lit/04-configuration.md#133
configStack :: IO Config
configStack = do
    localConfig <- readLocalConfig
    globalConfig <- readGlobalConfig
    return $ localConfig <> globalConfig <> defaultConfig
-- ------ end
-- ------ begin <<config-defaults>>[0] project://lit/04-configuration.md#145
hashComment         = ConfigComment "#"    Nothing
lispStyleComment    = ConfigComment ";"    Nothing
cStyleComment       = ConfigComment "/*"   (Just "*/")
cppStyleComment     = ConfigComment "//"   Nothing
haskellStyleComment = ConfigComment "--"   Nothing
mlStyleComment      = ConfigComment "(*"   (Just "*)")
xmlStyleComment     = ConfigComment "<!--" (Just "-->")
texStyleComment     = ConfigComment "%"    Nothing

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
-- ------ end
-- ------ begin <<config-input>>[0] project://lit/04-configuration.md#194
findFileAscending :: String -> IO (Maybe FilePath)
findFileAscending filename = do
    path <- dropTrailingPathSeparator <$> getCurrentDirectory
    let parents = reverse $ scanl1 (</>) $ splitDirectories path
    findFile parents filename

readLocalConfig :: IO Config
readLocalConfig = do
    cfg_path <- maybe (throwM $ SystemError "no config found.") id
             <$> findFileAscending "entangled.dhall"
    input auto (T.pack cfg_path)

readGlobalConfig :: IO Config
readGlobalConfig = mempty
-- ------ end
-- ------ begin <<config-reader>>[0] project://lit/04-configuration.md#213
lookupLanguage :: Config -> Text -> Maybe ConfigLanguage
lookupLanguage cfg x
    = find (elem x . languageIdentifiers) 
    $ configLanguages cfg

languageFromName :: Config -> Text -> Maybe ConfigLanguage
languageFromName cfg x
    = find ((== x) . languageName)
    $ configLanguages cfg
-- ------ end

getDatabasePath :: (MonadIO m, MonadThrow m) => Config -> m FilePath
getDatabasePath cfg = do
    dbPath <- case configDatabase cfg of
        Nothing -> throwM $ SystemError $ "database not configured"
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    return dbPath

getInputFiles :: (MonadIO m) => Config -> m [FilePath]
getInputFiles cfg = liftIO $ maybe mempty
        (concatMapM (glob . T.unpack))
        (configWatchList cfg)
-- ------ end
