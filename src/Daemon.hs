-- ------ language="Haskell" file="src/Daemon.hs" project://lit/10-daemon.md
-- ------ begin <<daemon>>[0] project://src/Daemon.hs#3
module Daemon where

import Prelude hiding (writeFile, readFile)

-- ------ begin <<daemon-imports>>[0] project://lit/10-daemon.md
-- ------ begin <<import-text>>[0] project://lit/01-entangled.md
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import qualified Data.Text.IO as T.IO
-- ------ begin <<import-map>>[0] project://lit/01-entangled.md
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
-- ------ end
import TextUtil (tshow, unlines')
-- ------ end
-- ------ begin <<daemon-imports>>[1] project://lit/10-daemon.md
import qualified System.FSNotify as FSNotify
-- ------ end
-- ------ begin <<daemon-imports>>[2] project://src/Daemon.hs#12
import Database.SQLite.Simple

import Document
import Config
import Database
import Tangle (parseMarkdown, expandedCode, Annotator)
import Comment
import Stitch (stitch)
import Transaction
import FileIO

import qualified Data.Text.Prettyprint.Doc as P
import qualified Console
import Console (Doc)

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.IO.Class
import Control.Monad.Writer
import Control.Monad.Catch
import Control.Monad.Logger
-- ------ end
-- ------ begin <<daemon-imports>>[3] project://lit/10-daemon.md
import Data.List (nub, (\\))
import Control.Monad (mapM)
-- ------ end
-- ------ begin <<daemon-imports>>[4] project://lit/10-daemon.md
import qualified Data.Map.Lazy as LM
-- ------ end
-- ------ begin <<daemon-imports>>[5] project://lit/10-daemon.md
import System.IO (stdout, hFlush, hSetBuffering, BufferMode(..))
-- ------ end

import System.FilePath (takeDirectory, equalFilePath)
import System.Directory
    ( canonicalizePath
    , doesFileExist
    , createDirectoryIfMissing
    , makeRelativeToCurrentDirectory
    , removeFile )

-- ------ begin <<daemon-events>>[0] project://lit/10-daemon.md
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
-- ------ begin <<daemon-session>>[0] project://src/Daemon.hs#22
data Session = Session
    { watches       :: [FSNotify.StopListening]
    , manager       :: FSNotify.WatchManager
    , eventChannel  :: Chan Event
    , daemonState   :: MVar DaemonState
    , sqlite        :: Connection
    }

db :: ( MonadIO m, MonadState Session m, MonadLoggerIO m )
   => SQL a -> m a
db x = do
    conn <- gets sqlite
    runSQL conn x

newtype Daemon a = Daemon { unDaemon :: RWST Config Transaction Session (LoggingT IO) a }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadState Session
             , MonadReader Config, MonadWriter Transaction, MonadThrow, MonadLogger, MonadLoggerIO )

-- up for removal
tryReadFile :: MonadIO m => FilePath -> m (Maybe Text)
tryReadFile f = liftIO $ do
    exists <- doesFileExist f
    if exists
        then Just <$> T.IO.readFile f
        else return Nothing

changeFile :: (MonadIO m) => FilePath -> Text -> m Transaction
changeFile filename text = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory filename
    liftIO $ createDirectoryIfMissing True (takeDirectory filename)
    oldText <- tryReadFile filename
    case oldText of
        Just ot -> if ot /= text
            then return $ Transaction (Just $ T.IO.writeFile filename text)
                                   (Console.msgOverwrite rel_path) False
            else return mempty
        Nothing -> return $ Transaction (Just $ T.IO.writeFile filename text)
                                     (Console.msgCreate rel_path) False

removeIfExists :: FilePath -> Transaction
removeIfExists f =
    plan (do
        fileExists <- doesFileExist f
        when fileExists $ removeFile f)
    <> doc (Console.msgDelete f)

instance MonadFileIO Daemon where
    writeFile path text = tell =<< changeFile path text
    readFile path = liftIO $ T.IO.readFile path
    deleteFile path = tell $ removeIfExists path
-- ------ end
-- ------ begin <<daemon-session>>[1] project://lit/10-daemon.md
setDaemonState :: ( MonadIO m
                  , MonadState Session m )
               => DaemonState -> m ()
setDaemonState s = do
    state <- gets daemonState
    liftIO $ modifyMVar_ state (const $ return s)
-- ------ end
-- ------ begin <<daemon-loading>>[0] project://lit/10-daemon.md
loadSourceFile :: ( MonadFileIO m, MonadLogger m
                  , MonadReader Config m
                  , MonadState Session m
                  , MonadIO m, MonadLoggerIO m )
               => FilePath -> m ()
loadSourceFile abs_path = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
    doc'     <- readFile abs_path >>= parseMarkdown rel_path
    case doc' of
        Left err ->
            logErrorN $ "Error loading '" <> T.pack rel_path <> "': " <> tshow err
        Right doc ->
            db $ insertDocument rel_path doc
-- ------ end
-- ------ begin <<daemon-loading>>[1] project://lit/10-daemon.md
loadTargetFile :: ( MonadFileIO m, MonadLogger m
                  , MonadReader Config m
                  , MonadState Session m
                  , MonadIO m, MonadLoggerIO m )
               => FilePath -> m ()
loadTargetFile abs_path = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
    refs' <- readFile abs_path >>= stitch rel_path
    case refs' of
        Left err ->
            logErrorN $ "Error loading '" <> T.pack rel_path <> "':" <> tshow err
        Right refs ->
            db $ updateTarget refs
-- ------ end
-- ------ begin <<daemon-writing>>[0] project://lit/10-daemon.md
annotateComment' :: Config -> Annotator
annotateComment' cfg rmap rid = runReaderT (annotateComment rmap rid) cfg

writeTargetFile :: FilePath -> Daemon ()
writeTargetFile rel_path = do
    cfg <- ask
    refs <- db $ queryReferenceMap cfg
    let codes = expandedCode (annotateComment' cfg) refs
        tangleRef tgt lang = case codes LM.!? tgt of
            Nothing        -> logErrorN $ "Reference `" <> tshow tgt <> "` not found."
            Just (Left e)  -> logErrorN $ tshow e
            Just (Right t) -> writeFile rel_path $ unlines' [headerComment lang rel_path, t]

    tgt' <- db $ queryTargetRef rel_path
    case tgt' of
        Nothing  -> logErrorN $ "Target `" <> T.pack rel_path <> "` not found."
        Just (tgt, langName) -> do
                case languageFromName cfg langName of
                    Nothing -> logErrorN $ "Unknown language id " <> langName
                    Just lang -> tangleRef tgt lang
-- ------ end
-- ------ begin <<daemon-writing>>[1] project://lit/10-daemon.md
writeSourceFile :: FilePath -> Daemon ()
writeSourceFile rel_path = do
    content <- db $ stitchDocument rel_path
    writeFile rel_path content
-- ------ end
-- ------ begin <<daemon-watches>>[0] project://lit/10-daemon.md
passEvent :: MVar DaemonState -> Chan Event
          -> [FilePath] -> [FilePath] -> FSNotify.Event -> IO ()
passEvent _      _       _    _    FSNotify.Removed {} = return ()
passEvent state' channel srcs tgts fsEvent = do
    putStrLn $ show fsEvent
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
-- ------ begin <<daemon-watches>>[1] project://lit/10-daemon.md
setWatch :: Daemon ()
setWatch = do
    srcs <- db listSourceFiles >>= (liftIO . mapM canonicalizePath)
    tgts <- db listTargetFiles >>= (liftIO . mapM canonicalizePath)
    fsnotify <- gets manager
    channel  <- gets eventChannel

    let abs_dirs = nub $ map takeDirectory (srcs <> tgts)
    rel_dirs <- liftIO $ mapM makeRelativeToCurrentDirectory abs_dirs

    state <- gets daemonState
    stopActions <- liftIO $ mapM
        (\dir -> FSNotify.watchDir fsnotify dir (const True)
                                   (passEvent state channel srcs tgts))
        abs_dirs
    modify (\s -> s{ watches=stopActions })

    logInfoN $ "watching: " <> tshow rel_dirs
-- ------ end
-- ------ begin <<daemon-watches>>[2] project://lit/10-daemon.md
closeWatch :: Daemon ()
closeWatch = do
    stopActions <- gets watches
    liftIO $ sequence_ stopActions
    logInfoN "suspended watches"
-- ------ end
-- ------ begin <<daemon-main-loop>>[0] project://lit/10-daemon.md
wait :: Daemon ()
wait = liftIO $ threadDelay 100000

mainLoop :: Event -> Daemon ()
-- ------ begin <<main-loop-cases>>[0] project://lit/10-daemon.md
mainLoop (WriteSource abs_path) = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path

    wait
    setDaemonState Tangling
    closeWatch

    old_tgts <- db listTargetFiles
    loadSourceFile abs_path
    new_tgts <- db listTargetFiles
    mapM_ deleteFile $ old_tgts \\ new_tgts
    mapM_ writeTargetFile new_tgts

    wait
    setWatch
    setDaemonState Idle

mainLoop (WriteTarget abs_path) = do
    rel_path <- liftIO $ makeRelativeToCurrentDirectory abs_path
    wait
    setDaemonState Stitching
    closeWatch
    loadTargetFile abs_path
    srcs <- db listSourceFiles
    mapM_ writeSourceFile srcs
    wait
    -- setDaemonState Tangling
    -- tgts <- db listTargetFiles
    -- mapM_ writeTargetFile tgts
    -- wait
    setWatch
    setDaemonState Idle

mainLoop _ = return ()
-- ------ end
-- ------ end
-- ------ begin <<daemon-start>>[0] project://lit/10-daemon.md
printMsg :: Doc -> Daemon ()
printMsg = liftIO . Console.putTerminal

initSession :: Daemon ()
initSession = do
    config <- ask
    abs_paths <- liftIO $ getInputFiles config
    when (null abs_paths) $ throwM $ SystemError "No input files."
    rel_paths <- liftIO $ mapM makeRelativeToCurrentDirectory abs_paths

    db createTables

    printMsg Console.banner
    tell $ doc
         $ (P.align $ P.vsep
                   $ map (Console.bullet
                         . (P.pretty ("Monitoring " :: Text) <>)
                         . Console.fileRead)
                           rel_paths)
            <> P.line

    mapM_ loadSourceFile abs_paths
    tgts <- db listTargetFiles
    mapM_ writeTargetFile tgts
    setWatch

foldx :: (Monad m) => (e -> s -> m s) -> [e] -> s -> m ()
foldx _ [] _ = return ()
foldx f (e:es) s = f e s >>= foldx f es

runSession :: Config -> IO ()
runSession config = do
    hSetBuffering stdout LineBuffering
    fsnotify <- FSNotify.startManager
    channel <- newChan
    daemon_state <- newMVar Idle
    db_path <- getDatabasePath config
    let dbRunner conn = do
                    let session = Session [] fsnotify channel daemon_state conn
                    (x, session', action) <- runStdoutLoggingT $ runRWST (unDaemon initSession) config session
                    runTransaction action
                    events <- getChanContents channel
                    foldx (\e s -> do
                                      (_, s', a) <- runStdoutLoggingT $ runRWST (unDaemon $ mainLoop e) config s
                                      runTransaction a
                                      return s') events session'
    liftIO $ withConnection db_path dbRunner
    liftIO $ FSNotify.stopManager fsnotify
-- ------ end
-- ------ end
-- ------ end
