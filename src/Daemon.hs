-- ------ language="Haskell" file="src/Daemon.hs"
-- ------ begin <<daemon>>[0]
{-# LANGUAGE: TemplateHaskell #-}
module Daemon where

-- ------ begin <<daemon-imports>>[0]
import Lens.Micro.Platform
-- ------ end
-- ------ begin <<daemon-imports>>[1]
import qualified System.FSNotify as FSNotify
-- ------ end
-- ------ begin <<daemon-imports>>[2]
import System.Directory 
    ( canonicalizePath
    , doesFileExist
    , removeFile
    , createDirectoryIfMissing
    , makeRelativeToCurrentDirectory)
-- ------ end
-- ------ begin <<daemon-session>>[0]
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
-- ------ end
-- ------ begin <<daemon-files>>[0]
class Monad m => MonadUserIO m where
    overwrite :: FilePath -> Text -> m ()
    create :: FilePath -> Text -> m ()
    delete :: FilePath -> m ()
    notify :: LogLevel -> Text -> m ()
    read :: FilePath -> m Text
-- ------ end
-- ------ end
-- ------ end
