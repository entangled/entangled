# FileIO

## Transactions

``` {.haskell file=src/Transaction.hs}
module Transaction where

<<transaction-imports>>

data Transaction = Transaction
  { action :: Maybe (IO ())
  , description :: Doc
  , needConfirm :: Bool }

<<transaction>>
```

When an event happened we need to respond, usually by writing out several files. Since `IO` is a `Monoid`, we can append `IO` actions and keep track of describing the gathered events in a `Transaction`. There are some things that we may need to ask the user permission for, like overwriting files in dubious circumstances. Messaging is done through pretty-printed `Doc`.

``` {.haskell #transaction-imports}
import qualified Data.Text.Prettyprint.Doc as P
import Console (Doc)
import qualified Console
import Control.Monad.Logger (LogLevel)
import qualified Data.Text.IO as T.IO
import System.IO (stdout, hFlush)
import Control.Monad (when)
```

The `action` is wrapped in a `Maybe` so that we can tell if the `Transaction` does anything. A `Transaction` is a `Monoid`.

``` {.haskell #transaction}
instance Semigroup Transaction where
    (Transaction al dl cl) <> (Transaction ar dr cr)
      = Transaction (al <> ar) (dl <> dr) (cl || cr)

instance Monoid Transaction where
    mempty = Transaction mempty mempty False
```

We can build `Transaction`s by appending elemental parts.

``` {.haskell #transaction}
plan :: IO () -> Transaction
plan action = Transaction (Just action) mempty False

doc :: Doc -> Transaction
doc x = Transaction Nothing x False

msg :: P.Pretty a => LogLevel -> a -> Transaction
msg level doc = Transaction Nothing (Console.msg level doc) False

confirm :: Transaction
confirm = Transaction mempty mempty True
```

In most of the program logic, `Transaction` will be available in terms of a `MonadWriter`.

``` {.haskell #transaction}
runTransaction :: Transaction -> IO ()
runTransaction (Transaction Nothing d _) = Console.putTerminal d
runTransaction (Transaction (Just x) d c) = do
    Console.putTerminal d
    if c then do
        T.IO.putStr "confirm? (y/n) "
        hFlush stdout
        reply <- getLine
        T.IO.putStrLn ""
        when (reply == "y") x
    else x
```

## File transactions

There is a limited set of IO file system actions that result from a tangle or stitch. We define a little language using a type class.

- When a target gets created or modified, we need to `writeFile`.
- When a target is removed or renamed, we need to `deleteFile`.
- For every tangle and stitch operation we need to `readFile`.

The behaviour will be such that, if a file is deleted and no other file remains in its containing directory, the directory is removed. If we write a file to a directory that does not exists, the directory is created.

``` {.haskell file=src/FileIO.hs}
{-# LANGUAGE ScopedTypeVariables #-}
module FileIO where

<<file-io-imports>>

class Monad m => MonadFileIO m where
    writeFile :: FilePath -> Text -> m ()
    deleteFile :: FilePath -> m ()
    readFile :: FilePath -> m Text

<<file-io-prim>>
```

## Conversion
The `stdio` library defines its own `Text` datatype. I'm not ready to switch that deep. Issue #26 on github `haskell-stdio/stdio` gives a snippet that converts between `Bytes` and the more ubiquitous `ByteString`.

``` {.haskell #file-io-imports}
import qualified Std.Foreign.PrimArray  as F
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BU
import qualified Std.Data.CBytes        as CBytes
import           Std.Data.Vector (Bytes)
```

``` {.haskell #file-io-prim}
bytesToByteString :: Bytes -> IO BS.ByteString
bytesToByteString bytes =
  F.withPrimVectorSafe bytes (\ptr len -> BS.packCStringLen (F.castPtr ptr, len))

bytesFromByteString :: BS.ByteString -> IO Bytes
bytesFromByteString bs =
  CBytes.toBytes <$> BU.unsafeUseAsCString bs CBytes.fromCString
```

To this we can add conversion between `Text` and `Bytes`.

``` {.haskell #file-io-prim}
textToBytes :: Text -> IO Bytes
textToBytes = bytesFromByteString . T.Encoding.encodeUtf8 

bytesToText :: Bytes -> IO Text
bytesToText bytes = T.Encoding.decodeUtf8 <$> (bytesToByteString bytes)
```

## System calls

We use the `stdio` package, hosting the `Std.IO` module. This package is rather low-level, but it features the `fsync` function. We need this to be able to synchronize the IO and have full control over what file events we listen to. `stdio` is an FFI wrapper around `libuv`, which provides a portable interface to do (a)synchronous POSIX(y) IO.

``` {.haskell #file-io-imports}
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
```

### CBytes

``` {.haskell #file-io-imports}
import Std.Data.CBytes ( CBytes, pack )
```

We keep the conversion between `FilePath` and `CBytes` in a single place.

``` {.haskell #file-io-prim}
fromFilePath :: FilePath -> CBytes
fromFilePath = pack
```

### Stat

``` {.haskell #file-io-imports}
import Std.IO.FileSystem ( stat, UVStat(..), UVFileMode(..) )
import Std.IO.Exception ( NoSuchThing(..), InappropriateType(..), SomeIOException(..)
                        , Handler(..), ioeDescription, catches )
```

We will be using `stat` to identify directories from files. The `Std.IO` packages can throw exceptions. We distinguish between expectable exceptions in the proper use of the function and the ones that should cause an abort. In this case, not finding the file that we're trying to `stat` is within correct use of the `safeStat` function, while, for instance trying to `stat` a file to which we have no access should raise an exception.

``` {.haskell #file-io-prim}
safeStat :: (MonadIO m, MonadThrow m) => FilePath -> m (Maybe UVStat)
safeStat path = liftIO $
    (Just <$> stat (fromFilePath path)) `catches`
        [ Handler (\ (ex :: NoSuchThing) -> return Nothing)
        , Handler (\ (ex :: SomeIOException) -> throwM $
                     SystemError $ "failed `stat` on `" <> (T.pack path) <> "`") ]
```

Using `safeStat` we can figure out if a file exists and if it is a directory.

``` {.haskell #file-io-prim}
exists :: (MonadIO m, MonadThrow m) => FilePath -> m Bool
exists path = maybe False (const True) <$> (safeStat path)

isDir :: (MonadIO m, MonadThrow m) => FilePath -> m Bool
isDir path = maybe False checkMode <$> (safeStat path)
    where checkMode s = (stMode s .&. 0o170000) == 0o040000
```

Here we have hard-coded the masks (see `man 2 stat` and `man 7 inode`). These should be present in `Std.IO` as `S_IFMT` and `S_IFDIR` bit patterns.

### Make/Remove dir

The `select` function is a nice way of modelling a function that has non-local exits (see [Haskell Wiki if-then-else](https://wiki.haskell.org/If-then-else)); the first entry that evaluates to true yields the result. The `selectM` variant evaluates the predicates inside the same monad as the result.

In these functions `Dir` variants act on a single directory, while `Path` variant iterates through the parents to create `mkdir -p` behaviour.

``` {.haskell #file-io-imports}
import System.FilePath (splitDirectories, (</>))
import Std.IO.FileSystem ( mkdir, rmdir, scandir )
```

``` {.haskell #file-io-prim}
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
```

Strictly, the `rmdir` function only does anything if the directory is empty. Somehow I feel better explicitly checking if the directory is empty.

``` {.haskell #file-io-prim}
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
```

### Remove file

``` {.haskell #file-io-imports}
import Std.IO.FileSystem ( unlink )
```

``` {.haskell #file-io-prim}
removeFile :: (MonadIO m, MonadThrow m) => FilePath -> m ()
removeFile path = liftIO $
    (unlink (fromFilePath path)) `catches`
        [ Handler (\ (InappropriateType ioe :: InappropriateType) -> throwEx ioe)
        , Handler (\ (NoSuchThing ioe :: NoSuchThing) -> throwEx ioe) ]
    where throwEx ioe = throwM $ SystemError $ "could not remove `" 
            <> (T.pack path) <> "`, " <> (T.pack $ ioeDescription ioe)
```

### Write file

``` {.haskell #file-io-imports}
import Std.IO.Resource   ( withResource )
import Std.IO.FileSystem ( fsync, initUVFile, UVFileFlag(..) )
import Std.IO.Buffered   ( newBufferedInput, newBufferedOutput, defaultChunkSize
                         , readAll', writeBuffer )
```

``` {.haskell #file-io-prim}
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
```

## FileIO instance

``` {.haskell #user-io-imports}
import System.FilePath (takeDirectory)
```

``` {.haskell #user-io-instance}
newtype FileIO a = FileIO { unFileIO :: LoggingT IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadLogger, MonadLoggerIO)

instance MonadFileIO FileIO where
    writeFile path text = logInfoN $ "writing `" <> (T.pack path) <> "`"
                        >> ensurePath (takeDirectory path)
                        >> writeIfChanged path text

    deleteFile path     = logInfoN $ "deleting `" <> (T.pack path) <> "`"
                        >> removeFile path
                        >> rmPathIfEmpty (takeDirectory path)

    readFile path       = logInfoN $ "reading `" <> (T.pack path) <> "`"
                        >> T.IO.readFile path
```

``` {.haskell #user-io}
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

```

These are IO actions that need logging, possible confirmation by the user and execution. Also, using this we can do some mock testing.

