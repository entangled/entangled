module Daemon
    ( listAllTargetFiles ) where

import System.INotify
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M
import Data.List
import Data.Either

import Control.Monad.Reader
import Control.Monad.State

import Model
import Config
import Tangle
import Untangle
import Parser

data Session = Session
    { sourceData   :: M.Map FilePath [Content]
    , referenceMap :: ReferenceMap
    }

setReferenceMap :: Monad m => ReferenceMap -> StateT Session m ()
setReferenceMap r = modify (\(Session s _) -> Session s r)

getDocument :: Monad m => FilePath -> StateT Session m (Maybe Document)
getDocument p = do
    content <- M.lookup p <$> gets sourceData
    refs    <- gets referenceMap
    return $ Document refs <$> content

listAllTargetFiles :: Monad m => StateT Session m [FilePath]
listAllTargetFiles = map referenceName . filter isFileReference . M.keys <$> gets referenceMap

writeFileOrWarn :: Show a => FilePath -> Either a String -> IO ()
writeFileOrWarn filename (Left error)
    = putStrLn $ "Error tangling '" ++ filename ++ "': " ++ show error
writeFileOrWarn filename (Right text) = writeFile filename text

tangleDocument :: Document -> ReaderT Config IO ()
tangleDocument doc = do
    fileMap <- tangleAnnotated doc
    liftIO $ mapM_ (uncurry writeFileOrWarn) (M.toList fileMap)

untangleTarget :: FilePath -> ReaderT Config IO (Either TangleError ReferenceMap)
untangleTarget f = liftIO (readFile f) >>= untangle f

loadSourceFile :: FilePath -> ReaderT Config IO (Either TangleError Document)
loadSourceFile f = do
    source  <- liftIO $ readFile f
    parseMarkdown f source

addSourceFile :: FilePath -> StateT Session (ReaderT Config IO) ()
addSourceFile f = do
    source  <- liftIO $ readFile f
    refs    <- gets referenceMap
    doc'    <- parseMarkdown' refs f source
    case doc' of
        Left err -> liftIO $ putStrLn "Error loading '" ++ f ++ "': "
                        ++ show err
        Right doc -> do
            setReferenceMap $ references doc
            addFileContent  $ documentContent doc

-- createSession :: [FilePath] -> IO Session
-- createSession fs = 

runDaemon :: IO ()
runDaemon = do
    inotify <- initINotify
    killINotify inotify
