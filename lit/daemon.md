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
{-# LANGUAGE: TemplateHaskell #-}
module Daemon where

<<daemon-imports>>
<<daemon-session>>
<<daemon-files>>
```

``` {.haskell #daemon-imports}
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

``` {.haskell #daemon-session}
data Session = Session
    { _sourceData    :: Map FilePath [Content]
    , _referenceMap  :: ReferenceMap
    , _watches       :: [FSNotify.StopListening]
    , _manager       :: FSNotify.WatchManager
    , _eventChannel  :: Chan Event
    , _daemonState   :: MVar DaemonState
    }

makeLenses ''Session

listAllSourceFiles :: ( MonadState Session m ) => m [FilePath]
listAllSourceFiles = use $ sourceData . to M.keys

listAllTargetFiles :: ( MonadState Session m ) => m [FilePath]
listAllTargetFiles = use $ referenceMap . to getTargets
```

Every time an event happens we send it to `_eventChannel`. When we tangle we change the `_daemonState` to `Tangling`. Write events to target files are then not triggering a stitch. The other way around, if the daemon is in `Stitching` state, write events to the markdown source do not trigger a tangle. Because these events will arrive asynchronously, we use an `MVar` to change the state.

### File IO

``` {.haskell #daemon-imports}
import System.Directory 
    ( canonicalizePath
    , doesFileExist
    , removeFile
    , createDirectoryIfMissing
    , makeRelativeToCurrentDirectory)
```

There is a limited set of IO file system actions that result from a tangle or stitch. We define a little language using a type class.

``` {.haskell #daemon-files}
class Monad m => MonadUserIO m where
    overwrite :: FilePath -> Text -> m ()
    create :: FilePath -> Text -> m ()
    delete :: FilePath -> m ()
    notify :: LogLevel -> Text -> m ()
    read :: FilePath -> m Text
```

These are IO actions that need logging, possible confirmation by the user and execution.

## Watching

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

    let isSourceFile = any (equalFilePath path) srcs
        isTargetFile = any (equalFilePath path) tgts
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
    srcs <- listSourceFiles >>= (mapM canonicalizePath)
    tgts <- listTargetFiles >>= (mapM canonicalizePath)
    fsnotify <- use manager
    channel  <- use eventChannel

    let abs_dirs = nub $ map takeDirectory (srcs <> tgts)
    rel_dirs <- mapM makeRelativeToCurrentDirectory abs_dirs

    state <- use daemonState
    stopActions <- mapM
        (\dir -> FSNotify.watchDir fsnotify dir (const True)
                                   (passEvent state channel srcs tgts))
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
    sequence_ stopActions
    notify Message "suspended watches"
```

