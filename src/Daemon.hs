{-# LANGUAGE FlexibleContexts #-}

module Daemon
    ( runSession ) where

import System.Directory ( canonicalizePath
                        , doesFileExist
                        , removeFile
                        , createDirectoryIfMissing
                        , makeRelativeToCurrentDirectory)
import Control.Concurrent.Chan
import Control.Concurrent
import System.FilePath (takeDirectory, equalFilePath)
import System.IO

import qualified System.FSNotify as FSNotify
import Data.Function (on)

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import qualified Data.Map as M
import Data.List
import Data.Either
import Data.Maybe
import Data.Traversable
import qualified Data.Foldable as F
import System.Random

import Control.Monad.Reader
import Control.Monad.State

import Lens.Micro.Platform

import Model (TangleError, toTangleError)
import Config
import Tangle
import Untangle
import Markdown
import Document

import Console (ConsoleT, LogLevel(..), Doc)
import qualified Console
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

data FileType = SourceFile | TargetFile deriving (Show, Eq, Ord)
data DaemonState = Idle | Tangling | Untangling deriving (Show, Eq)
data Event = WriteEvent FileType FilePath
           | DebugEvent T.Text
           deriving (Show)

data IOAction = IOAction (Maybe (IO ())) Doc

instance Semigroup IOAction where
    (IOAction al dl) <> (IOAction ar dr) = IOAction (al <> ar) (dl <> dr)

instance Monoid IOAction where
    mempty = IOAction mempty mempty

msg :: P.Pretty a => Console.LogLevel -> a -> IOAction
msg Error m = IOAction (Just $ return ()) $ Console.msg Error m <> P.line
msg Warning m = IOAction (Just $ return ()) $ Console.msg Warning m <> P.line
msg level m = IOAction mempty $ Console.msg level m <> P.line

msgOverwrite = IOAction mempty . Console.msgOverwrite
msgDelete = IOAction mempty . Console.msgDelete
msgCreate = IOAction mempty . Console.msgCreate
msgGroup h (IOAction x d) = IOAction x $ Console.group h d

plan :: IO () -> IOAction
plan action = IOAction (Just action) mempty

printMsg :: Doc -> TangleM ()
printMsg = liftIO . Console.putTerminal

run :: IOAction -> TangleM ()
run (IOAction x d) = liftIO $
    case x of
        Nothing -> return ()
        Just x' -> do { Console.putTerminal d; x' }
        

foldMapM :: (Traversable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f lst = F.fold <$> mapM f lst

-- ========================================================================= --
-- Session                                                                   --
-- ========================================================================= --

data Session = Session
    { _sourceData     :: M.Map FilePath [Content]
    , _referenceMap   :: ReferenceMap
    , _watches        :: [FSNotify.StopListening]
    , fsNotifyManager :: FSNotify.WatchManager
    , _eventChannel   :: Chan Event
    , _daemonState    :: MVar DaemonState
    , _randomGen      :: StdGen
    }

type TangleM = StateT Session (ReaderT Config IO)

sourceData :: Lens' Session (M.Map FilePath [Content])
sourceData = lens _sourceData (\s n -> s { _sourceData = n })

referenceMap :: Lens' Session ReferenceMap
referenceMap = lens _referenceMap (\s n -> s { _referenceMap = n })

watches :: Lens' Session [FSNotify.StopListening]
watches = lens _watches (\s n -> s { _watches = n })

eventChannel :: Lens' Session (Chan Event)
eventChannel = lens _eventChannel (\s n -> s { _eventChannel = n })

daemonState :: Lens' Session (MVar DaemonState)
daemonState = lens _daemonState (\s n -> s { _daemonState = n })

randomGen :: Lens' Session StdGen
randomGen = lens _randomGen (\s n -> s { _randomGen = n })

instance RandomGen Session where
    next s  = (s ^. randomGen ^. to next)
            & _2 %~ (\ x -> s & randomGen .~ x)
    split s = (s ^. randomGen ^. to split)
            & _1 %~ (\ x -> s & randomGen .~ x)
            & _2 %~ (\ x -> s & randomGen .~ x)

setReferenceMap :: Monad m => ReferenceMap -> StateT Session m ()
setReferenceMap r = modify (set referenceMap r)

addFileContent :: Monad m => FilePath -> [Content] -> StateT Session m ()
addFileContent f c = modify (over sourceData (M.insert f c))

updateReferenceMap :: Monad m => (ReferenceMap -> ReferenceMap) -> StateT Session m ()
updateReferenceMap f = modify (over referenceMap f)

getDocument :: Monad m => FilePath -> StateT Session m (Maybe Document)
getDocument p = do
    content <- use $ sourceData . to (M.lookup p)
    refs    <- use referenceMap
    return $ Document refs <$> content

listAllTargetFiles :: Monad m => StateT Session m [FilePath]
listAllTargetFiles = map referenceName . filter isFileReference . M.keys <$> use referenceMap

listAllSourceFiles :: Monad m => StateT Session m [FilePath]
listAllSourceFiles = use $ sourceData . to M.keys

-- ========================================================================= --
-- Tangling                                                                  --
-- ========================================================================= --

tryReadFile :: MonadIO m => FilePath -> m (Maybe T.Text)
tryReadFile f = liftIO $ do
    exists <- doesFileExist f
    if exists
        then Just <$> T.IO.readFile f
        else return Nothing

changeFile :: (MonadIO m) => FilePath -> T.Text -> m IOAction
changeFile filename text = do
    shortPath <- liftIO $ makeRelativeToCurrentDirectory filename
    liftIO $ createDirectoryIfMissing True (takeDirectory filename)
    oldText <- tryReadFile filename
    case oldText of
        Just ot -> if ot /= text
            then return $ plan (T.IO.writeFile filename text)
                <> msgOverwrite shortPath
            else return mempty
        Nothing -> return $ plan (T.IO.writeFile filename text)
                   <> msgCreate shortPath

removeIfExists :: FilePath -> IOAction
removeIfExists f =
    plan (do fileExists <- doesFileExist f
             when fileExists $ removeFile f)
    <> msgDelete f

removeFiles :: [FilePath] -> IOAction
removeFiles = foldMap removeIfExists

writeFileOrWarn :: (MonadIO m, Show a) => FilePath -> Either a T.Text -> m IOAction
writeFileOrWarn filename (Left error) = do
    shortPath <- liftIO $ makeRelativeToCurrentDirectory filename
    return $ msg Error $ "Error tangling '" ++ shortPath ++ "': " ++ show error
writeFileOrWarn filename (Right text) =
    changeFile filename text

tangleTargets :: TangleM IOAction
tangleTargets = do
    refs <- use referenceMap
    fileMap <- lift $ tangleAnnotated refs
    foldMapM (uncurry writeFileOrWarn) (M.toList fileMap)

-- ========================================================================= --
-- Loading                                                                   --
-- ========================================================================= --

loadSourceFile :: FilePath -> TangleM (Either TangleError Document)
loadSourceFile f = do
    source  <- liftIO $ T.IO.readFile f
    parseMarkdown f source

addSourceFile :: FilePath -> TangleM IOAction
addSourceFile f = do
    shortPath <- liftIO $ makeRelativeToCurrentDirectory f
    source  <- liftIO $ T.IO.readFile f
    refs    <- use referenceMap
    doc'    <- parseMarkdown' refs f source
    case doc' of
        Left err -> return $ msg Error $
            "Error loading '" ++ shortPath ++ "': " ++ show err
        Right (Document r c) -> do
            setReferenceMap r
            addFileContent f c
            return mempty

removeActiveReferences :: Document -> TangleM ()
removeActiveReferences doc = do
    let rs = listActiveReferences doc
    mapM_ (modifying referenceMap . M.delete) rs

updateFromSource :: FilePath -> TangleM IOAction
updateFromSource fp = do
    shortPath <- liftIO $ makeRelativeToCurrentDirectory fp
    doc' <- loadSourceFile fp
    case doc' of
        Left err -> return $ msg Error $
            "Error loading '" ++ shortPath ++ "': " ++ show err
        Right (Document r c) -> do
            modifying referenceMap (M.union r)
            modifying sourceData (M.insert fp c)
            return mempty

-- ========================================================================= --
-- Untangle                                                                  --
-- ========================================================================= --

untangleTarget :: MonadIO m => FilePath -> ReaderT Config m (Either TangleError ReferenceMap)
untangleTarget f = do
    shortPath <- liftIO $ makeRelativeToCurrentDirectory f
    content <- liftIO $ readFile f
    untangle shortPath content

getCodeBlock :: Monad m => ReferenceId -> StateT Session m (Maybe CodeBlock)
getCodeBlock id = use $ referenceMap . to (M.lookup id)

updateCodeBlock :: ReferenceId -> CodeBlock -> TangleM IOAction
updateCodeBlock r c = do
    old' <- getCodeBlock r
    case old' of
        Nothing -> return $ msg Error $
            "Error: code block " ++ show r ++ " not present."
        Just old -> if (codeSource old) /= (codeSource c)
            then do
                updateReferenceMap $ M.insert r c
                return $ msg Message $ "updated " <> show r
            else return mempty

updateFromTarget :: FilePath -> TangleM IOAction
updateFromTarget f = do
    shortPath <- liftIO $ makeRelativeToCurrentDirectory f
    refs' <- lift $ untangleTarget f
    case refs' of
        Left err -> return $ msg Error $ show err
        Right refs -> foldMapM (uncurry updateCodeBlock) (M.toList refs)

-- ========================================================================= --
-- Stitching                                                                 --
-- ========================================================================= --

stitchSourceFile :: FilePath -> TangleM IOAction
stitchSourceFile f = do
    doc' <- getDocument f
    case doc' of
        Nothing  -> return mempty
        Just doc -> changeFile f (stitchText doc)

stitchSources :: TangleM IOAction
stitchSources = do
    srcs <- listAllSourceFiles
    foldMapM stitchSourceFile srcs

-- ========================================================================= --
-- Watching                                                                  --
-- ========================================================================= --

passEvent :: MVar DaemonState -> Chan Event -> [FilePath] -> [FilePath] -> FSNotify.Event -> IO ()
passEvent _  _       _    _    FSNotify.Removed {} = return ()
passEvent ds channel srcs tgts fsEvent = do
    path <- canonicalizePath $ FSNotify.eventPath fsEvent
    relpath <- makeRelativeToCurrentDirectory path
    ds' <- readMVar ds
    let isSourceFile = any (equalFilePath path) srcs
        isTargetFile = any (equalFilePath path) tgts
        pass = case ds' of
                    Idle -> isSourceFile || isTargetFile
                    Tangling -> isSourceFile
                    Untangling -> isTargetFile
    when pass $ do
        let filetype = if isSourceFile then SourceFile else TargetFile
            event = WriteEvent filetype path
        -- putStrLn $ "\027[33m----- :state: " ++ show ds' ++ " :event: " ++ show event ++ " -----\027[m"
        writeChan channel event

setWatch :: TangleM IOAction
setWatch = do
    srcs <- listAllSourceFiles >>= (liftIO . mapM canonicalizePath)
    tgts <- listAllTargetFiles >>= (liftIO . mapM canonicalizePath)

    fsnotify <- gets fsNotifyManager
    channel  <- use eventChannel

    let dirs = nub $ map takeDirectory (srcs ++ tgts)
    reldirs <- liftIO $ mapM makeRelativeToCurrentDirectory dirs
    ds <- use daemonState
    stopActions <- liftIO $ mapM
        (\dir -> FSNotify.watchDir fsnotify dir (const True)
                                   (passEvent ds channel srcs tgts))
        dirs
    assign watches stopActions
    return $ msg Message $ "watching: " <> show reldirs

removeWatch :: TangleM IOAction
removeWatch = do
    w <- use watches
    liftIO $ sequence_ w
    return $ msg Message "suspended watches."

-- ========================================================================= --
-- Main interface                                                            --
-- ========================================================================= --

setDaemonState :: DaemonState -> TangleM ()
setDaemonState s = do
    ds <- use daemonState
    liftIO $ modifyMVar_ ds (const $ return s)

wait :: TangleM ()
wait = liftIO $ threadDelay 100000

mainLoop :: [Event] -> TangleM ()

mainLoop [] = return ()

mainLoop (WriteEvent SourceFile fp : xs) = do
    shortPath <- liftIO $ makeRelativeToCurrentDirectory fp
    wait
    setDaemonState Tangling
    doc <- fromJust <$> getDocument fp  -- TODO: do proper error handling here
    oldTgtFiles <- listAllTargetFiles
    removeActiveReferences doc
    x <- updateFromSource fp
    y <- tangleTargets
    newTgtFiles <- listAllTargetFiles 
    run $ msgGroup (P.pretty "Tangling" P.<+> Console.fileRead shortPath)
        $ x <> y <> removeFiles (oldTgtFiles \\ newTgtFiles)
    wait
    removeWatch >>= run
    setWatch >>= run
    setDaemonState Idle
    mainLoop xs

mainLoop (WriteEvent TargetFile fp : xs) = do
    shortPath <- liftIO $ makeRelativeToCurrentDirectory fp
    setDaemonState Untangling
    wait            -- the write event may arrive before the editor
                    -- has finished saving the document
    x <- updateFromTarget fp
    y <- stitchSources
    run $ msgGroup (P.pretty "Untangling" P.<+> Console.fileRead shortPath)
        $ x <> y
    wait
    setDaemonState Idle
    mainLoop xs

mainLoop (DebugEvent msg : xs)   = do
    liftIO $ putStrLn $ "Debug: " ++ show msg
    mainLoop xs

startSession :: [FilePath] -> TangleM ()
startSession fs = do
    shortPaths <- liftIO $ mapM makeRelativeToCurrentDirectory fs
    printMsg Console.banner

    let w = IOAction (Just mempty) 
                $ (P.align $ P.vsep
                   $ map (Console.bullet
                         . (<> P.line)
                         . (P.pretty "Monitoring " <> )
                         . Console.fileRead
                         ) shortPaths)
    x <- foldMapM addSourceFile fs
    y <- tangleTargets
    run $ msgGroup (P.pretty "Initializing")
        $ w <> x <> y
    setWatch >>= run
    eventList' <- use $ eventChannel . to getChanContents
    eventList  <- liftIO eventList'
    mainLoop eventList

runSession :: Config -> [FilePath] -> IO ()
runSession cfg fs = do
    hSetBuffering stdout LineBuffering
    fs' <- mapM canonicalizePath fs
    fsnotify <- liftIO FSNotify.startManager
    channel <- newChan
    ds <- newMVar Idle
    rnd <- getStdGen
    let session = Session M.empty M.empty [] fsnotify channel ds rnd
    runReaderT (runStateT (startSession fs') session) cfg
    liftIO $ FSNotify.stopManager fsnotify
