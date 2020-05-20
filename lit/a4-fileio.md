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
{-# LANGUAGE NoImplicitPrelude #-}
module FileIO where

import RIO
<<file-io-imports>>

class Monad m => MonadFileIO m where
    writeFile :: FilePath -> Text -> m ()
    deleteFile :: FilePath -> m ()
    readFile :: FilePath -> m Text

<<file-io-prim>>
<<file-io-instance>>
```

## Primitives

``` {.haskell #file-io-imports}
import RIO.Text (Text)
import qualified RIO.Text as T

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, throwM)

import Errors (EntangledError(SystemError))
import Select (selectM)
import TextUtil (tshow)
```

### Make/Remove dir

``` {.haskell #file-io-imports}
import RIO.Directory ( createDirectoryIfMissing, doesDirectoryExist
                     , listDirectory, removeFile, removeDirectory )
import RIO.FilePath  ( (</>), splitDirectories )
import RIO.List      ( scanl1 )
```

``` {.haskell #file-io-prim}
ensurePath :: (MonadIO m, MonadLogger m) => FilePath -> m ()
ensurePath path = selectM (return ())
    [ ( not <$> doesDirectoryExist path
      , logInfoN ("creating directory `" <> (T.pack path) <> "`")
        >> createDirectoryIfMissing True path ) ]
```

``` {.haskell #file-io-prim}
rmDirIfEmpty :: (MonadIO m, MonadThrow m, MonadLogger m) => FilePath -> m ()
rmDirIfEmpty path = selectM (return ())
    [ ( not <$> doesDirectoryExist path
      , throwM $ SystemError $ "could not remove dir: `" <> (T.pack path) <> "`")
    , ( null <$> listDirectory path
      , logInfoN ("removing empty directory `" <> (T.pack path) <> "`")
        >> removeDirectory path ) ]

parents :: FilePath -> [FilePath]
parents = scanl1 (</>) . splitDirectories

rmPathIfEmpty :: (MonadIO m, MonadThrow m, MonadLogger m) => FilePath -> m ()
rmPathIfEmpty = mapM_ rmDirIfEmpty . reverse . parents
```

### Write file

``` {.haskell #file-io-imports}
import RIO.File ( writeBinaryFileDurable )
import qualified RIO.ByteString as B
import Control.Exception ( IOException )
```

``` {.haskell #file-io-prim}
writeIfChanged :: (MonadIO m, MonadThrow m, MonadLogger m) => FilePath -> Text -> m ()
writeIfChanged path text = do
    old_content' <- liftIO $ try $ B.readFile path
    case (old_content' :: Either IOException B.ByteString) of
        Right old_content | old_content == new_content -> return ()
                          | otherwise                  -> write
        Left  _                                        -> write
    where new_content = T.encodeUtf8 text
          write       = logInfoN ("writing `" <> (T.pack path) <> "`")
                      >> writeBinaryFileDurable path new_content
```

## FileIO instance

``` {.haskell #file-io-imports}
import RIO.FilePath         ( takeDirectory )
import Control.Monad.Logger ( MonadLogger, MonadLoggerIO, LoggingT, logInfoN
                            , runStderrLoggingT )
```

``` {.haskell #file-io-instance}
newtype FileIO a = FileIO { unFileIO :: LoggingT IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadLogger, MonadLoggerIO)

runFileIO :: ( MonadIO m ) => FileIO a -> m a
runFileIO = liftIO . runStderrLoggingT . unFileIO

instance MonadFileIO FileIO where
    writeFile path text = ensurePath (takeDirectory path)
                        >> writeIfChanged path text

    deleteFile path     = logInfoN ("deleting `" <> (T.pack path) <> "`")
                        >> removeFile path
                        >> rmPathIfEmpty (takeDirectory path)

    readFile path       = logInfoN ("reading `" <> (T.pack path) <> "`")
                        >> readFile path
```

These are IO actions that need logging, possible confirmation by the user and execution. Also, using this we can do some mock testing.

