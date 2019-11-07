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

import Prelude hiding (writeFile, readFile)

<<daemon-imports>>
<<daemon-events>>
<<daemon-io-action>>
<<daemon-session>>
<<daemon-user-io>>
<<daemon-loading>>
<<daemon-writing>>
<<daemon-watches>>
<<daemon-main-loop>>
```

``` {.haskell #daemon-imports}
<<import-text>>
import qualified Data.Text.IO as T.IO 
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

### Aggregate IO

``` {.haskell #daemon-io-action}
data IOAction = IOAction
  { action :: Maybe (IO ())
  , description :: Doc
  , needConfirm :: Bool }

instance Semigroup IOAction where
    (IOAction al dl cl) <> (IOAction ar dr cr)
      = IOAction (al <> ar) (dl <> dr) (cl || cr)

instance Monoid IOAction where
    mempty = IOAction mempty mempty False
```

### Session

``` {.haskell #daemon-imports}
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
```

``` {.haskell #daemon-session}
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
class Monad m => MonadFileIO m where
    writeFile :: FilePath -> Text -> m ()
    deleteFile :: FilePath -> m ()
    readFile :: FilePath -> m Text
```

These are IO actions that need logging, possible confirmation by the user and execution. Also, using this we can do some mock testing.

## Watching

``` {.haskell #daemon-imports}
import Data.List (nub, (\\))
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
```

``` {.haskell #daemon-watches}
closeWatch :: Daemon ()
closeWatch = do
    stopActions <- use watches
    liftIO $ sequence_ stopActions
    logMessage "suspended watches"
```

## Loading

``` {.haskell #daemon-loading}
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
```

``` {.haskell #daemon-loading}
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
```

## Writing

``` {.haskell #daemon-imports}
import qualified Data.Map.Lazy as LM
```

``` {.haskell #daemon-writing}
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
```

``` {.haskell #daemon-writing}
writeSourceFile :: FilePath -> Daemon ()
writeSourceFile rel_path = do
    content <- db $ stitchDocument rel_path
    writeFile rel_path content
```

## Main loop

The `mainLoop` is fed events and handles them.

``` {.haskell #daemon-main-loop}
class ( MonadIO m, MonadReader Config m, MonadState Session m )
      => MonadEntangled m

mainLoop :: [Event] -> Daemon ()
<<main-loop-cases>>
```

``` {.haskell #main-loop-cases}
mainLoop [] = return ()
```

After the first event we need to wait a bit, there may be more comming.

``` {.haskell #main-loop-cases}
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
```
