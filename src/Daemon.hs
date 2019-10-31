-- ------ language="Haskell" file="src/Daemon.hs"
-- ------ begin <<daemon>>[0]
{-# LANGUAGE TemplateHaskell #-}
module Daemon where

-- ------ begin <<daemon-imports>>[0]
import Prelude hiding (readFile)
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ begin <<import-map>>[0]
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
-- ------ end
import TextUtil (tshow)
import Lens.Micro.Platform
-- ------ end
-- ------ begin <<daemon-imports>>[1]
import qualified System.FSNotify as FSNotify
-- ------ end
-- ------ begin <<daemon-imports>>[2]
import Document
import Config
import Database
import Database.SQLite.Simple
import Tangle (parseMarkdown)
import Stitch (stitch)
import Console (LogLevel(..))

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class
-- ------ end
-- ------ begin <<daemon-imports>>[3]
import System.FilePath (takeDirectory, equalFilePath)
import System.Directory 
    ( canonicalizePath
    , doesFileExist
    , removeFile
    , createDirectoryIfMissing
    , makeRelativeToCurrentDirectory )
-- ------ end
-- ------ begin <<daemon-imports>>[4]
import Data.List (nub)
import Control.Monad (mapM)
-- ------ end
-- ------ begin <<daemon-events>>[0]
data DaemonState
    = Idle
    | Tangling
    | Stitching
    deriving (Show, Eq)

data Event
    = WriteSource FilePath
    | WriteTarget FilePath
    | DebugEvent Text
    deriving (Show)
-- ------ end
-- ------ begin <<daemon-session>>[0]
data Session = Session
    { _watches       :: [FSNotify.StopListening]
    , _manager       :: FSNotify.WatchManager
    , _eventChannel  :: Chan Event
    , _daemonState   :: MVar DaemonState
    , _sqlite        :: Connection
    }

makeLenses ''Session

db :: ( MonadIO m, MonadState Session m )
   => SQL a -> m a
db (SQL x) = use sqlite >>= liftIO . runReaderT x

newtype Daemon = Daemon { unDaemon :: RWST Config () Session IO }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadState Session
             , MonadReader Config )
-- ------ end
-- ------ begin <<daemon-session>>[1]
setDaemonState :: ( MonadIO m
                  , MonadState Session m )
               => DaemonState -> m ()
setDaemonState s = do
    state <- use daemonState
    liftIO $ modifyMVar_ state (const $ return s)
-- ------ end
-- ------ begin <<daemon-user-io>>[0]
class Monad m => MonadUserIO m where
    overwrite :: FilePath -> Text -> m ()
    create :: FilePath -> Text -> m ()
    delete :: FilePath -> m ()
    notify :: LogLevel -> Text -> m ()
    readFile :: FilePath -> m Text
-- ------ end
-- ------ begin <<daemon-loading>>[0]
-- loadSourceFile :: ( MonadUserIO m
--                   , MonadReader Config m
--                   , MonadState Session m
--                   , MonadIO m )
--                => FilePath -> m ()
-- loadSourceFile abs_path = do
--     rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
--     doc'     <- readFile abs_path >>= parseMarkdown rel_path
--     case doc' of
--         Left err ->
--             notify Error $ "Error loading '" <> T.pack rel_path <> "': " <> tshow err
--         Right doc@(Document refs content files) -> do
--             modify (over sourceData (M.insert abs_path (documentContent doc)))
-- ------ end
-- ------ begin <<daemon-loading>>[1]
-- loadTargetFile :: ( MonadUserIO m
--                   , MonadReader Config m
--                   , MonadState Session m
--                   , MonadIO m )
--                => FilePath -> m ()
-- loadTargetFile abs_path = do
--     rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
--     refs' <- readFile abs_path >>= stitch rel_path
--     case refs' of
--         Left err ->
--             notify Error $ "Error loading '" <> T.pack rel_path <> "':" <> tshow err
--         Right refs ->
--             updateReferences refs
-- 
-- allReferences :: ( MonadState Session m )
--               => m ReferenceMap
-- allReferences = M.unions <$> use (over sourceFiles (map references . M.values))
-- 
-- updateReferenceMap :: ( MonadState Session m
--                       , MonadUserIO m )
--                    => [ReferencePair] -> m ()
-- updateReferenceMap =
--     either (notify Error . tshow)
--            (assign allReferences)
--            <$> use allReferences >>= foldM updateReference
--     where updateReference refs (ref, code)
--               | ref `M.member` refs = Right $ M.insert ref code refs
--               | otherwise = Left $ StitchError "Unknown reference '" <> tshow ref <> "'"
-- ------ end
-- ------ begin <<daemon-watches>>[0]
passEvent :: MVar DaemonState -> Chan Event
          -> [FilePath] -> [FilePath] -> FSNotify.Event -> IO ()
passEvent _      _       _    _    FSNotify.Removed {} = return ()
passEvent state' channel srcs tgts fsEvent = do
    abs_path <- canonicalizePath $ FSNotify.eventPath fsEvent
    state    <- readMVar state'

    let isSourceFile = any (equalFilePath abs_path) srcs
        isTargetFile = any (equalFilePath abs_path) tgts
        pass         = case state of
                         Idle      -> isSourceFile || isTargetFile
                         Tangling  -> isSourceFile
                         Stitching -> isTargetFile

    when pass $ do
        let etype = if isSourceFile then WriteSource else WriteTarget
        writeChan channel (etype abs_path)
-- ------ end
-- ------ begin <<daemon-watches>>[1]
setWatch :: ( MonadIO m
            , MonadState Session m
            , MonadUserIO m )
         => m ()
setWatch = do
    srcs <- db listSourceFiles >>= (liftIO . mapM canonicalizePath)
    tgts <- db listTargetFiles >>= (liftIO . mapM canonicalizePath)
    fsnotify <- use manager
    channel  <- use eventChannel

    let abs_dirs = nub $ map takeDirectory (srcs <> tgts)
    rel_dirs <- liftIO $ mapM makeRelativeToCurrentDirectory abs_dirs

    state <- use daemonState
    stopActions <- liftIO $ mapM
        (\dir -> FSNotify.watchDir fsnotify dir (const True)
                                   (passEvent state channel srcs tgts))
        abs_dirs
    assign watches stopActions

    notify Message $ "watching: " <> tshow rel_dirs
-- ------ end
-- ------ begin <<daemon-watches>>[2]
closeWatch :: ( MonadIO m
              , MonadState Session m
              , MonadUserIO m )
           => m ()
closeWatch = do
    stopActions <- use watches
    liftIO $ sequence_ stopActions
    notify Message "suspended watches"
-- ------ end
-- ------ end
-- ------ end
