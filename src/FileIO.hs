-- ------ language="Haskell" file="src/FileIO.hs" project://lit/a4-fileio.md
{-# LANGUAGE ScopedTypeVariables #-}
module FileIO where

-- ------ begin <<file-io-imports>>[0] project://lit/a4-fileio.md
import qualified Std.Foreign.PrimArray  as F
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BU
import qualified Std.Data.CBytes        as CBytes
import           Std.Data.Vector (Bytes)
-- ------ end
-- ------ begin <<file-io-imports>>[1] project://lit/a4-fileio.md
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Bits ((.&.), (.|.))

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Exception (displayException)

import Errors (EntangledError(SystemError))
import Select (selectM)
import TextUtil (tshow)
-- ------ end
-- ------ begin <<file-io-imports>>[2] project://lit/a4-fileio.md
import Std.Data.CBytes ( CBytes, pack )
-- ------ end
-- ------ begin <<file-io-imports>>[3] project://lit/a4-fileio.md
import Std.IO.FileSystem ( stat, UVStat(..), UVFileMode(..) )
import Std.IO.Exception ( NoSuchThing(..), InappropriateType(..), SomeIOException(..)
                        , Handler(..), ioeDescription, catches )
-- ------ end
-- ------ begin <<file-io-imports>>[4] project://lit/a4-fileio.md
import System.FilePath (splitDirectories, (</>))
import Std.IO.FileSystem ( mkdir, rmdir, scandir )
-- ------ end
-- ------ begin <<file-io-imports>>[5] project://lit/a4-fileio.md
import Std.IO.FileSystem ( unlink )
-- ------ end
-- ------ begin <<file-io-imports>>[6] project://lit/a4-fileio.md
import Std.IO.Resource   ( withResource )
import Std.IO.FileSystem ( fsync, initUVFile, UVFileFlag(..) )
import Std.IO.Buffered   ( newBufferedInput, newBufferedOutput, defaultChunkSize
                         , readAll', writeBuffer )
-- ------ end

class Monad m => MonadFileIO m where
    writeFile :: FilePath -> Text -> m ()
    deleteFile :: FilePath -> m ()
    readFile :: FilePath -> m Text

-- ------ begin <<file-io-prim>>[0] project://lit/a4-fileio.md
bytesToByteString :: Bytes -> IO BS.ByteString
bytesToByteString bytes =
  F.withPrimVectorSafe bytes (\ptr len -> BS.packCStringLen (F.castPtr ptr, len))

bytesFromByteString :: BS.ByteString -> IO Bytes
bytesFromByteString bs =
  CBytes.toBytes <$> BU.unsafeUseAsCString bs CBytes.fromCString
-- ------ end
-- ------ begin <<file-io-prim>>[1] project://lit/a4-fileio.md
textToBytes :: Text -> IO Bytes
textToBytes = bytesFromByteString . T.Encoding.encodeUtf8

bytesToText :: Bytes -> IO Text
bytesToText bytes = T.Encoding.decodeUtf8 <$> (bytesToByteString bytes)
-- ------ end
-- ------ begin <<file-io-prim>>[2] project://lit/a4-fileio.md
fromFilePath :: FilePath -> CBytes
fromFilePath = pack
-- ------ end
-- ------ begin <<file-io-prim>>[3] project://lit/a4-fileio.md
safeStat :: (MonadIO m, MonadThrow m) => FilePath -> m (Maybe UVStat)
safeStat path = liftIO $
    (Just <$> stat (fromFilePath path)) `catches`
        [ Handler (\ (ex :: NoSuchThing) -> return Nothing)
        , Handler (\ (ex :: SomeIOException) -> throwM $
                     SystemError $ "failed `stat` on `" <> (T.pack path) <> "`") ]
-- ------ end
-- ------ begin <<file-io-prim>>[4] project://lit/a4-fileio.md
exists :: (MonadIO m, MonadThrow m) => FilePath -> m Bool
exists path = maybe False (const True) <$> (safeStat path)

isDir :: (MonadIO m, MonadThrow m) => FilePath -> m Bool
isDir path = maybe False checkMode <$> (safeStat path)
    where checkMode s = (stMode s .&. 0o170000) == 0o040000
-- ------ end
-- ------ begin <<file-io-prim>>[5] project://lit/a4-fileio.md
ensureDir :: (MonadIO m, MonadThrow m) => FilePath -> m ()
ensureDir path =
    selectM (throwM $ SystemError $ "cannot create dir: `" <> (T.pack path)
                                <> "`, exists and is a file")
            [ (isDir path,          return ())
            , (not <$> exists path, liftIO $ mkdir (fromFilePath path) mode) ]
    where mode = 0o0775

parents :: FilePath -> [FilePath]
parents = scanl1 (</>) . splitDirectories

ensurePath :: (MonadIO m, MonadThrow m) => FilePath -> m ()
ensurePath = mapM_ ensureDir . parents
-- ------ end
-- ------ begin <<file-io-prim>>[6] project://lit/a4-fileio.md
isEmptyDir :: (MonadIO m, MonadThrow m) => FilePath -> m Bool
isEmptyDir path =
    selectM (return False)
            [ (not <$> isDir path, return False)
            , (null <$> ls,        return True) ]
    where ls = liftIO $ scandir (fromFilePath path)

rmDirIfEmpty :: (MonadIO m, MonadThrow m) => FilePath -> m ()
rmDirIfEmpty path =
    selectM (throwM $ SystemError $ "could not remove dir: `"
                   <> (T.pack path) <> "` not a directory")
            [ (isEmptyDir path, liftIO $ rmdir (fromFilePath path))
            , (isDir path,      return ()) ]

rmPathIfEmpty :: (MonadIO m, MonadThrow m) => FilePath -> m ()
rmPathIfEmpty = mapM_ rmDirIfEmpty . reverse . parents
-- ------ end
-- ------ begin <<file-io-prim>>[7] project://lit/a4-fileio.md
removeFile :: (MonadIO m, MonadThrow m) => FilePath -> m ()
removeFile path = liftIO $
    (unlink (fromFilePath path)) `catches`
        [ Handler (\ (InappropriateType ioe :: InappropriateType) -> throwEx ioe)
        , Handler (\ (NoSuchThing ioe :: NoSuchThing) -> throwEx ioe) ]
    where throwEx ioe = throwM $ SystemError $ "could not remove `"
            <> (T.pack path) <> "`, " <> (T.pack $ ioeDescription ioe)
-- ------ end
-- ------ begin <<file-io-prim>>[8] project://lit/a4-fileio.md
writeIfChanged :: (MonadIO m, MonadThrow m) => FilePath -> Text -> m ()
writeIfChanged path text = liftIO $ withResource openFile $ \file -> do
        input <- newBufferedInput file defaultChunkSize
        old_content <- readAll' input
        new_content <- textToBytes text
        if old_content == new_content
            then return ()
            else do
                output <- newBufferedOutput file defaultChunkSize
                writeBuffer output new_content
        fsync file
    where openFile = initUVFile (fromFilePath path) (O_CREAT .|. O_RDWR) DEFAULT_MODE
-- ------ end
-- ------ end
