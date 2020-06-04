-- ~\~ language=Haskell filename=src/Config.hs
-- ~\~ begin <<lit/04-configuration.md|src/Config.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Config where

import RIO hiding (void)
import qualified RIO.Map as M
-- ~\~ begin <<lit/04-configuration.md|config-import>>[0]
import Dhall (FromDhall, ToDhall, input, auto, Decoder, record, list
             , field, setFromDistinctList, constructor, unit, union)
import qualified Data.Text as T
-- ~\~ end

import Errors
import qualified Format
import Data.List (find, scanl1)
import Control.Monad.Extra (concatMapM)
import System.FilePath.Glob (glob)
import System.Directory
import System.FilePath

-- ~\~ begin <<lit/04-configuration.md|config-dhall-schema>>[0]
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
    a == b = languageName a == languageName b

instance Ord ConfigLanguage where
    compare a b = compare (languageName a) (languageName b)

decodeFormatSpec :: Decoder Format.Spec
decodeFormatSpec = fromMaybe [Format.Plain "illegal format spec"] . Format.spec <$> auto

lineDirectivesDecoder :: Decoder (Map Text Format.Spec)
lineDirectivesDecoder = M.fromList <$> list entry
    where entry = record ( pair <$> field "name" auto
                                <*> field "format" decodeFormatSpec )
          pair a b = (a, b)

data AnnotateMethod = AnnotateNaked
                    | AnnotateStandard
                    | AnnotateProject
                    deriving (Show, Eq)

annotateDecoder :: Decoder AnnotateMethod
annotateDecoder = union
        (  ( AnnotateNaked          <$ constructor "Naked" unit )
        <> ( AnnotateStandard       <$ constructor "Standard" unit )
        <> ( AnnotateProject        <$ constructor "Project" unit ) )

data Config = Config
    { configVersion   :: Text
    , configLanguages :: Set ConfigLanguage
    , configWatchList :: [Text]
    , configDatabase  :: Maybe Text
    , configAnnotate  :: AnnotateMethod
    , configLineDirectives :: Map Text Format.Spec
    , configUseLineDirectives :: Bool
    } deriving (Show)

configDecoder :: Decoder Config
configDecoder = record
    ( Config <$> field "version" auto
             <*> field "languages" (setFromDistinctList configLanguage)
             <*> field "watchList" auto
             <*> field "database" auto
             <*> field "annotate" annotateDecoder
             <*> field "lineDirectives" lineDirectivesDecoder
             <*> field "useLineDirectives" auto
    )

class HasConfig env where
    config :: Lens' env Config
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-input>>[0]
findFileAscending :: String -> IO (Maybe FilePath)
findFileAscending filename = do
    path <- dropTrailingPathSeparator <$> getCurrentDirectory
    let parents = reverse $ scanl1 (</>) $ splitDirectories path
    findFile parents filename

readLocalConfig :: IO Config
readLocalConfig = do
    cfg_path <- findFileAscending "entangled.dhall"
            >>= maybe (throwM $ SystemError "no config found") return
    input configDecoder $ "(" <> T.pack cfg_path <> ").entangled"
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

getDatabasePath :: (MonadIO m, MonadThrow m) => Config -> m FilePath
getDatabasePath cfg = do
    dbPath <- case configDatabase cfg of
        Nothing -> throwM $ SystemError "database not configured"
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    return dbPath

getInputFiles :: (MonadIO m) => Config -> m [FilePath]
getInputFiles cfg = liftIO $ concatMapM (glob . T.unpack) (configWatchList cfg)
-- ~\~ end
