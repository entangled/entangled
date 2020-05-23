-- ~\~ language=Haskell filename=src/Daemon.hs
-- ~\~ begin <<lit/10-daemon.md|src/Daemon.hs>>[0]
-- ~\~ begin <<lit/10-daemon.md|daemon>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Daemon where

import RIO
import RIO.List (sort)
import qualified RIO.Text as T

-- ~\~ begin <<lit/10-daemon.md|daemon-imports>>[0]
import qualified System.FSNotify as FSNotify
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-imports>>[1]
import Database.SQLite.Simple (Connection)
import Database (db, connection, HasConnection, listSourceFiles, listTargetFiles)

-- import Transaction
-- import FileIO
import Tangle (annotateComment')
import Entangled
import Config (Config, HasConfig, config, getInputFiles, configWatchList)
import Errors (EntangledError(..))

import Console (Doc, putTerminal)
import qualified Console
import qualified Data.Text.Prettyprint.Doc as P

-- import Control.Concurrent.Chan
-- import Control.Concurrent
-- import Control.Monad.Catch
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-imports>>[2]
import Data.List (nub)
-- import Control.Monad (mapM)
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-imports>>[3]
-- import System.IO (stdout, hFlush, hSetBuffering, BufferMode(..))
import RIO.Directory (makeRelativeToCurrentDirectory, canonicalizePath)
import RIO.FilePath (equalFilePath, takeDirectory)
-- import Control.Exception (IOException)
-- ~\~ end

-- ~\~ begin <<lit/10-daemon.md|daemon-events>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-session>>[0]
data Session = Session
    { watches       :: MVar [FSNotify.StopListening]
    , manager       :: FSNotify.WatchManager
    , eventChannel  :: Chan Event
    , daemonState   :: MVar DaemonState
    , connection'   :: Connection
    , config'       :: Config
    , logFunc'      :: LogFunc
    }

instance HasConfig Session where
    config = lens config' (\x y -> x { config' = y })

instance HasLogFunc Session where
    logFuncL = lens logFunc' (\x y -> x { logFunc' = y })

instance HasConnection Session where
    connection = lens connection' (\x y -> x { connection' = y })

newtype Daemon a = Daemon { unDaemon :: RIO Session a }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadReader Session, MonadThrow, MonadUnliftIO )
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-session>>[1]
setDaemonState :: DaemonState -> Daemon ()
setDaemonState s = do
    state <- asks daemonState
    modifyMVar_ state (const $ return s)
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-watches>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-watches>>[1]
setWatch :: Daemon ()
setWatch = do
    srcs <- db listSourceFiles >>= (liftIO . mapM canonicalizePath)
    tgts <- db listTargetFiles >>= (liftIO . mapM canonicalizePath)
    fsnotify <- asks manager
    channel  <- asks eventChannel

    let abs_dirs = nub $ map takeDirectory (srcs <> tgts)
    rel_dirs <- mapM makeRelativeToCurrentDirectory abs_dirs

    state <- asks daemonState
    stopActions <- liftIO $ mapM
        (\dir -> FSNotify.watchDir fsnotify dir (const True)
                                   (passEvent state channel srcs tgts))
        abs_dirs
    watchesMVar <- asks watches
    putMVar watchesMVar stopActions

    logDebug $ display $ "watching: " <> tshow rel_dirs
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-watches>>[2]
closeWatch :: Daemon ()
closeWatch = do
    stopActions <- takeMVar =<< asks watches
    liftIO $ sequence_ stopActions
    logDebug "suspended watches"
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-main-loop>>[0]
mainLoop :: Event -> Daemon ()
-- ~\~ begin <<lit/10-daemon.md|main-loop-cases>>[0]
mainLoop (WriteSource abs_path) = do
    rel_path <- makeRelativeToCurrentDirectory abs_path
    logDebug $ display $ "tangle triggered on `" <> T.pack rel_path <> "`"
    cfg <- view config

    setDaemonState Tangling
    closeWatch

    runEntangled $ do
        insertSources [rel_path]
        tangle TangleAll (annotateComment' cfg)
        clearOrphans

    setWatch
    setDaemonState Idle

mainLoop (WriteTarget abs_path) = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
    logDebug $ display $ "stitch triggered on `" <> T.pack rel_path <> "`"
    setDaemonState Stitching
    closeWatch

    runEntangled $ do
        insertTargets [rel_path]
        stitch StitchAll

    setWatch
    setDaemonState Idle

mainLoop _ = return ()
-- ~\~ end
-- ~\~ end
-- ~\~ begin <<lit/10-daemon.md|daemon-start>>[0]
printMsg :: Doc -> Daemon ()
printMsg = liftIO . Console.putTerminal

initSession :: Daemon ()
initSession = do
    cfg <- view config
    abs_paths <- sort <$> getInputFiles cfg
    when (null abs_paths) $ throwM $ SystemError "No input files."
    rel_paths <- mapM makeRelativeToCurrentDirectory abs_paths

    printMsg Console.banner
    printMsg $ P.align (P.vsep
                   $ map (Console.bullet
                         . (P.pretty ("Monitoring " :: Text) <>)
                         . Console.fileRead)
                           rel_paths)
             <> P.line

    runEntangled $ do
        insertSources rel_paths
        tangle TangleAll (annotateComment' cfg)

    setWatch

runSession :: (HasConfig env, HasLogFunc env, HasConnection env, MonadReader env m, MonadIO m)
           => [FilePath] -> m ()
runSession inputFiles = do
    hSetBuffering stdout LineBuffering

    cfg' <- view config
    let cfg = cfg' { configWatchList = configWatchList cfg' <> map T.pack inputFiles }
    conn <- view connection
    logFunc <- view logFuncL
    fsnotify <- liftIO FSNotify.startManager
    channel <- newChan
    daemonState' <- newMVar Idle
    watches' <- newEmptyMVar

    let session = Session watches' fsnotify channel daemonState' conn cfg logFunc
    runRIO session $ unDaemon $ do
        initSession
        mapM_ mainLoop =<< getChanContents channel

    liftIO $ FSNotify.stopManager fsnotify
-- ~\~ end
-- ~\~ end
-- ~\~ end
