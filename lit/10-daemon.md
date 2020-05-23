# The Daemon

The Entangled daemon does the following:

* Never crash
* Monitor markdown files, tangle when written to
* Monitor target files, stitch when written to
* Pretty print events

``` {.haskell file=src/Daemon.hs}
<<daemon>>
```

## Strategy

``` {.haskell #daemon}
{-# LANGUAGE NoImplicitPrelude #-}
module Daemon where

import RIO

<<daemon-imports>>

<<daemon-events>>
<<daemon-session>>
<<daemon-watches>>
<<daemon-main-loop>>
<<daemon-start>>
```

Using lenses we build a `Session` record. In combination with the `State` monad (and a `Reader Config`, and `IO`) we can manage the Entangled Daemon.

### FSNotify

We use `System.FSNotify` to setup watches on the files.

``` {.haskell #daemon-imports}
import qualified System.FSNotify as FSNotify
```

The watches are set to trigger events of type `Event`.

``` {.haskell #daemon-events}
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
```

### Session

``` {.haskell #daemon-imports}
import Database.SQLite.Simple (Connection)
import Database (db, connection, HasConnection, listSourceFiles, listTargetFiles)

-- import Transaction
-- import FileIO
import Tangle (annotateComment')
import Entangled
import Config (Config, HasConfig, config, getInputFiles)
import Errors (EntangledError(..))

import Console (Doc, putTerminal)
import qualified Console
import qualified Data.Text.Prettyprint.Doc as P

-- import Control.Concurrent.Chan
-- import Control.Concurrent
-- import Control.Monad.Catch
```

``` {.haskell #daemon-session}
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
```

Every time an event happens we send it to `_eventChannel`. When we tangle we change the `_daemonState` to `Tangling`. Write events to target files are then not triggering a stitch. The other way around, if the daemon is in `Stitching` state, write events to the markdown source do not trigger a tangle. Because these events will arrive asynchronously, we use an `MVar` to change the state.

Setting the state is a bit involved (a bit more than with an `IORef`), but it guarantees safe use in a multi-threaded environment. An `MVar` can only be set if it is first emptied, the combined action can be done with `modifyMVar_`.

``` {.haskell #daemon-session}
setDaemonState :: DaemonState -> Daemon ()
setDaemonState s = do
    state <- asks daemonState
    modifyMVar_ state (const $ return s)
```

## Watching

``` {.haskell #daemon-imports}
import Data.List (nub)
-- import Control.Monad (mapM)
```

The `passEvent` function acts as the call-back for the FSNotify watcher. There are two strategies in which editors save files:

    * remove and create: Vim (by default), gedit and many more editors do this. Actually, the content is written to a temporary file, which is then renamed to the existing file. These operations are atomic so that no data is lost if the system crashes.
    * modify: VS Code does this.

We have to be flexible in how we interpret the incomming events. The most important bit is that we need to ignore the `delete` events. 

`FSNotify` lets us watch directories containing the files we're interested in. In `passEvent` we need to check if the event is actually on an involved file.

``` {.haskell #daemon-watches}
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
```

``` {.haskell #daemon-watches}
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
```

``` {.haskell #daemon-watches}
closeWatch :: Daemon ()
closeWatch = do
    stopActions <- takeMVar =<< asks watches
    liftIO $ sequence_ stopActions
    logDebug "suspended watches"
```

## Loading

## Writing

## Main loop

``` {.haskell #daemon-imports}
-- import System.IO (stdout, hFlush, hSetBuffering, BufferMode(..))
import RIO.Directory (makeRelativeToCurrentDirectory, canonicalizePath)
import RIO.FilePath (equalFilePath, takeDirectory)
-- import Control.Exception (IOException)
```

The `mainLoop` is fed events and handles them.

``` {.haskell #daemon-main-loop}
mainLoop :: Event -> Daemon ()
<<main-loop-cases>>
```

After the first event we need to wait a bit, there may be more comming.

``` {.haskell #main-loop-cases}
mainLoop (WriteSource abs_path) = do
    rel_path <- makeRelativeToCurrentDirectory abs_path
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
    setDaemonState Stitching
    closeWatch

    runEntangled $ do
        insertTargets [rel_path]
        stitch StitchAll

    setWatch
    setDaemonState Idle

mainLoop _ = return () 
```

## Initialisation

``` {.haskell #daemon-start}
printMsg :: Doc -> Daemon ()
printMsg = liftIO . Console.putTerminal

initSession :: Daemon ()
initSession = do
    cfg <- view config
    abs_paths <- getInputFiles cfg
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
           => m ()
runSession = do
    hSetBuffering stdout LineBuffering

    cfg <- view config
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
```
