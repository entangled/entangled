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

We'll use `Lens` with template Haskell for convenience. That being said, I'm not a fan of the more cryptic unreadable junk that's possible with `lens`. Where possible I use the wordy functions over the alien gibberish in operator form.

``` {.haskell #daemon}
{-# LANGUAGE TemplateHaskell #-}
module Daemon where

<<daemon-imports>>
<<daemon-events>>
<<daemon-session>>
<<daemon-user-io>>
<<daemon-loading>>
<<daemon-watches>>
```

``` {.haskell #daemon-imports}
<<import-text>>
<<import-map>>
import TextUtil (tshow)
import Lens.Micro.Platform
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
import Document
import Console (LogLevel(..))

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.State
import Control.Monad.IO.Class
```

``` {.haskell #daemon-session}
data Session = Session
    { _sourceData    :: Map FilePath Document
    , _targetFiles   :: Map FilePath ReferenceName
    , _referenceMap  :: ReferenceMap
    , _watches       :: [FSNotify.StopListening]
    , _manager       :: FSNotify.WatchManager
    , _eventChannel  :: Chan Event
    , _daemonState   :: MVar DaemonState
    }

makeLenses ''Session

listSourceFiles :: ( MonadState Session m ) => m [FilePath]
listSourceFiles = use $ sourceData . to M.keys

listTargetFiles :: ( MonadState Session m ) => m [FilePath]
listTargetFiles = use $ targetFiles . to M.keys
```

Every time an event happens we send it to `_eventChannel`. When we tangle we change the `_daemonState` to `Tangling`. Write events to target files are then not triggering a stitch. The other way around, if the daemon is in `Stitching` state, write events to the markdown source do not trigger a tangle. Because these events will arrive asynchronously, we use an `MVar` to change the state.

Setting the state is a bit involved (a bit more than with an `IORef`), but it guarantees safe use in a multi-threaded environment. An `MVar` can only be set if it is first emptied, the combined action can be done with `modifyMVar_`.

``` {.haskell #daemon-session}
setDaemonState :: ( MonadIO m
                  , MonadState Session m )
               => DaemonState -> m ()
setDaemonState s = do
    state <- use daemonState
    liftIO $ modifyMVar_ state (const $ return s)
```

### File IO

``` {.haskell #daemon-imports}
import System.FilePath (takeDirectory, equalFilePath)
import System.Directory 
    ( canonicalizePath
    , doesFileExist
    , removeFile
    , createDirectoryIfMissing
    , makeRelativeToCurrentDirectory )
```

There is a limited set of IO file system actions that result from a tangle or stitch. We define a little language using a type class.

``` {.haskell #daemon-user-io}
class Monad m => MonadUserIO m where
    overwrite :: FilePath -> Text -> m ()
    create :: FilePath -> Text -> m ()
    delete :: FilePath -> m ()
    notify :: LogLevel -> Text -> m ()
    read :: FilePath -> m Text
```

These are IO actions that need logging, possible confirmation by the user and execution.

## Watching

``` {.haskell #daemon-imports}
import Data.List (nub)
import Control.Monad (mapM)
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
setWatch :: ( MonadIO m
            , MonadState Session m
            , MonadUserIO m )
         => m ()
setWatch = do
    srcs <- listSourceFiles >>= (liftIO . mapM canonicalizePath)
    tgts <- listTargetFiles >>= (liftIO . mapM canonicalizePath)
    fsnotify <- use manager
    channel  <- use eventChannel

    let abs_dirs = nub $ map takeDirectory (srcs <> tgts)
    rel_dirs <- liftIO $ mapM makeRelativeToCurrentDirectory abs_dirs

    state <- use daemonState
    stopActions <- liftIO $ mapM
        (\dir -> FSNotify.watchDir fsnotify dir (const True)
                                   (passEvent state channel srcs tgts))
        abs_dirs\
    assign watches stopActions

    notify Message $ "watching: " <> tshow rel_dirs
```

``` {.haskell #daemon-watches}
closeWatch :: ( MonadIO m
              , MonadState Session m
              , MonadUserIO m )
           => m ()
closeWatch = do
    stopActions <- use watches
    liftIO $ sequence_ stopActions
    notify Message "suspended watches"
```

## Loading

``` {.haskell #daemon-loading}
loadSourceFile :: ( MonadUserIO m
                  , MonadReader Config m )
               => FilePath -> m (Either EntangledError Document)
loadSourceFile path = read path >>= parseMarkdown path
```

``` {.haskell #daemon-loading}
addSourceFile :: ( MonadUserIO m
                 , MonadReader Config m
                 , MonadState Session m )
              => FilePath -> m ()
addSourceFile abs_path = do
    doc'     <- loadSourceFile
    case doc' of
        Left err -> notify Error $ "Error loading '" <> rel_path <> "': " <> tshow err
        Right doc@(Document refs content files) -> do
            updateReferenceMap refs
            modify (over sourceData (M.insert abs_path doc))
```

## Main loop

The `mainLoop` is fed events and handles them.

``` {.haskell #daemon-main-loop}
class ( MonadIO m, MonadReader Config m, MonadState Session m )
      => MonadEntangled m

type Entangled m = RWST Config () Session IO

mainLoop :: [Event] -> Entangled ()
<<main-loop-cases>>
```

``` {.haskell #main-loop-cases}
mainLoop [] = return ()
```

After the first event we need to wait a bit, there may be more comming.

``` {.haskell #main-loop-cases}
mainLoop (WriteSource abs_path : xs) = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
    wait
    setDaemonState Tangling
    doc <- fromJust <$> getDocument abs_path    -- TODO: handle errors
    old_tgts <- listTargetFiles
    
```
