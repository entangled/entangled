-- ------ language="Haskell" file="src/Daemon.hs"
-- ------ begin <<daemon>>[0]
{-# LANGUAGE TemplateHaskell #-}
module Daemon where

import Prelude hiding (writeFile, readFile)

-- ------ begin <<daemon-imports>>[0]
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import qualified Data.Text.IO as T.IO 
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
import Database.SQLite.Simple

import Document
import Config
import Database
import Logging
import Tangle (parseMarkdown, expandedCode)
import Comment
import Stitch (stitch)
import Console

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.IO.Class
import Control.Monad.Writer
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
import Data.List (nub, (\\))
import Control.Monad (mapM)
-- ------ end
-- ------ begin <<daemon-imports>>[5]
import qualified Data.Map.Lazy as LM
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
-- ------ begin <<daemon-io-action>>[0]
data IOAction = IOAction
  { action :: Maybe (IO ())
  , description :: Doc
  , needConfirm :: Bool }

instance Semigroup IOAction where
    (IOAction al dl cl) <> (IOAction ar dr cr)
      = IOAction (al <> ar) (dl <> dr) (cl || cr)

instance Monoid IOAction where
    mempty = IOAction mempty mempty False
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

db :: ( MonadIO m, MonadState Session m, MonadLogger m )
   => SQL a -> m a
db x = do
    conn <- use sqlite
    runSQL conn x

newtype Daemon a = Daemon { unDaemon :: RWST Config IOAction Session IO a }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadState Session
             , MonadReader Config, MonadWriter IOAction )

removeIfExists :: FilePath -> IOAction
removeIfExists f = IOAction (Just $ do fileExists <- doesFileExist f
                                       when fileExists $ removeFile f)
                            (msgDelete f) False

instance MonadFileIO Daemon where
    writeFile path text = tell $ IOAction (Just (T.IO.writeFile path text))
                                          (msgOverwrite path) False
    readFile path = liftIO $ T.IO.readFile path
    deleteFile path = tell $ removeIfExists path

instance MonadLogger Daemon where
    logEntry level msg = liftIO $ T.IO.putStrLn msg
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
class Monad m => MonadFileIO m where
    writeFile :: FilePath -> Text -> m ()
    deleteFile :: FilePath -> m ()
    readFile :: FilePath -> m Text
-- ------ end
-- ------ begin <<daemon-loading>>[0]
loadSourceFile :: ( MonadFileIO m, MonadLogger m
                  , MonadReader Config m
                  , MonadState Session m
                  , MonadIO m )
               => FilePath -> m ()
loadSourceFile abs_path = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
    doc'     <- readFile abs_path >>= parseMarkdown rel_path
    case doc' of
        Left err ->
            logError $ "Error loading '" <> T.pack rel_path <> "': " <> tshow err
        Right doc ->
            db $ insertDocument rel_path doc
-- ------ end
-- ------ begin <<daemon-loading>>[1]
loadTargetFile :: ( MonadFileIO m, MonadLogger m
                  , MonadReader Config m
                  , MonadState Session m
                  , MonadIO m )
               => FilePath -> m ()
loadTargetFile abs_path = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
    refs' <- readFile abs_path >>= stitch rel_path
    case refs' of
        Left err ->
            logError $ "Error loading '" <> T.pack rel_path <> "':" <> tshow err
        Right refs ->
            db $ updateTarget refs
-- ------ end
-- ------ begin <<daemon-writing>>[0]
writeTargetFile :: FilePath -> Daemon ()
writeTargetFile rel_path = do
    cfg <- ask
    refs <- db $ queryReferenceMap cfg
    let codes = expandedCode annotateComment refs
        tangleRef tgt = case codes LM.!? tgt of
            Nothing        -> logError $ "Reference `" <> tshow tgt <> "` not found."
            Just (Left e)  -> logError $ tshow e
            Just (Right t) -> writeFile rel_path t
    
    tgt' <- db $ queryTargetRef rel_path
    case tgt' of
        Nothing  -> logError $ "Target `" <> T.pack rel_path <> "` not found."
        Just tgt -> tangleRef tgt
-- ------ end
-- ------ begin <<daemon-writing>>[1]
writeSourceFile :: FilePath -> Daemon ()
writeSourceFile rel_path = do
    content <- db $ stitchDocument rel_path
    writeFile rel_path content
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
setWatch :: Daemon ()
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

    logMessage $ "watching: " <> tshow rel_dirs
-- ------ end
-- ------ begin <<daemon-watches>>[2]
closeWatch :: Daemon ()
closeWatch = do
    stopActions <- use watches
    liftIO $ sequence_ stopActions
    logMessage "suspended watches"
-- ------ end
-- ------ begin <<daemon-main-loop>>[0]
class ( MonadIO m, MonadReader Config m, MonadState Session m )
      => MonadEntangled m

mainLoop :: [Event] -> Daemon ()
-- ------ begin <<main-loop-cases>>[0]
mainLoop [] = return ()
-- ------ end
-- ------ begin <<main-loop-cases>>[1]
mainLoop (WriteSource abs_path : xs) = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
    setDaemonState Tangling
    old_tgts <- db listTargetFiles
    loadSourceFile abs_path
    new_tgts <- db listTargetFiles
    mapM_ deleteFile $ old_tgts \\ new_tgts
    mapM_ writeTargetFile new_tgts
    setDaemonState Idle

mainLoop (WriteTarget abs_path : xs) = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
    setDaemonState Stitching
    loadTargetFile abs_path
    srcs <- db listSourceFiles
    mapM_ writeSourceFile srcs
    setDaemonState Tangling
    tgts <- db listTargetFiles
    mapM_ writeTargetFile tgts
    setDaemonState Idle

mainLoop (_ : xs) = return () 
-- ------ end
-- ------ end
-- ------ end
-- ------ end
