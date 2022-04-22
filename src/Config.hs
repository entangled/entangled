-- ~\~ language=Haskell filename=src/Config.hs
-- ~\~ begin <<lit/04-configuration.md|src/Config.hs>>[0]
module Config ( module Config
              , module Version_1_3_0
              , module Config.Record ) where

import RIO hiding (void)
import RIO.Directory
import RIO.FilePath
import RIO.List (find)
import qualified RIO.Text as T

import Dhall (input, auto)
import System.FilePath.Glob

import qualified Config.Version_1_0_0 as Version_1_0_0
import qualified Config.Version_1_2_0 as Version_1_2_0
import qualified Config.Version_1_3_0 as Version_1_3_0
import Config.Version_1_3_0 (update, Config(..))
import Config.Record

import Errors
import Select

-- ~\~ begin <<lit/04-configuration.md|config-input>>[0]
findFileAscending :: String -> IO (Maybe FilePath)
findFileAscending filename = do
    path <- dropTrailingPathSeparator <$> getCurrentDirectory
    let parents = reverse $ scanl1 (</>) $ splitDirectories path
    findFile parents filename

getVersion :: FilePath -> IO Text
getVersion path =
    input auto $ "(" <> T.pack path <> ").entangled.version"

readLocalConfig :: IO Config
readLocalConfig = do
    cfg_path <- findFileAscending "entangled.dhall"
            >>= maybe (throwM $ SystemError "no config found") return
    version <- getVersion cfg_path
    decoder <- select (throwM $ SystemError $ "unrecognized version string '" <> version <> "'")
        [ ( version == "1.0.0", return $ update <=< input Version_1_0_0.configDecoder )
        , ( version == "1.2.0", return $ update <=< input Version_1_2_0.configDecoder )
        , ( version == "1.3.0", return $ input Version_1_3_0.configDecoder ) ]
    decoder $ "(" <> T.pack cfg_path <> ").entangled"
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-reader>>[0]
lookupLanguage :: Config -> Text -> Maybe ConfigLanguage
lookupLanguage cfg x
    = find (elem x . languageIdentifiers)
    $ configLanguages cfg

languageFromName :: Config -> Text -> Maybe ConfigLanguage
languageFromName cfg x
    = find ((== x) . languageName)
    $ configLanguages cfg
-- ~\~ end

class HasConfig env where
    config :: Lens' env Config

getDatabasePath :: (MonadIO m, MonadThrow m) => Config -> m FilePath
getDatabasePath cfg = do
    dbPath <- case configDatabase cfg of
        Nothing -> return ":memory:"
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    return dbPath

getInputFiles :: (MonadIO m) => Config -> m [FilePath]
getInputFiles cfg = liftIO $ mconcat <$> mapM (glob . T.unpack) (configWatchList cfg)
-- ~\~ end
