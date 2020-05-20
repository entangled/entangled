-- ------ language="Haskell" file="src/Config.hs" project://lit/04-configuration.md
module Config where

-- ------ begin <<config-import>>[0] project://lit/04-configuration.md
import Dhall (Generic, FromDhall, ToDhall, input, auto, Decoder, union, record, field, list, strictText, setFromDistinctList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
-- ------ end

import Errors

import qualified Data.Text.IO as T.IO
import TextUtil
-- ------ begin <<import-set>>[0] project://lit/01-entangled.md
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

-- ------ begin <<config-dhall-schema>>[0] project://lit/04-configuration.md
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
    } deriving (Show)

configLanguage :: Decoder ConfigLanguage
configLanguage = record
    ( ConfigLanguage <$> field "name"        auto
                     <*> field "identifiers" auto
                     <*> field "comment"     auto
    )

instance Eq ConfigLanguage where
    a == b = (languageName a) == (languageName b)

instance Ord ConfigLanguage where
    compare a b = compare (languageName a) (languageName b)

data Config = Config
    { configLanguages :: Set ConfigLanguage
    , configWatchList :: [Text]
    , configDatabase  :: Maybe Text
    } deriving (Show)

config :: Decoder Config
config = record
    ( Config <$> field "languages" (setFromDistinctList configLanguage)
             <*> field "watchList" auto
             <*> field "database" auto
    )
-- ------ end
-- ------ begin <<config-input>>[0] project://lit/04-configuration.md
findFileAscending :: String -> IO (Maybe FilePath)
findFileAscending filename = do
    path <- dropTrailingPathSeparator <$> getCurrentDirectory
    let parents = reverse $ scanl1 (</>) $ splitDirectories path
    findFile parents filename

readLocalConfig :: IO Config
readLocalConfig = do
    cfg_path <- maybe (throwM $ SystemError "no config found.") id
             <$> findFileAscending "entangled.dhall"
    input config $ "(" <> T.pack cfg_path <> ").entangled"
-- ------ end
-- ------ begin <<config-reader>>[0] project://lit/04-configuration.md
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
        Nothing -> throwM $ SystemError "database not configured"
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    return dbPath

getInputFiles :: (MonadIO m) => Config -> m [FilePath]
getInputFiles cfg = liftIO $ concatMapM (glob . T.unpack) (configWatchList cfg)
-- ------ end
