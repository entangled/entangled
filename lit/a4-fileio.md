# Transactions
The idea here is that we'd like to create an abstract class to do all our file system interactions. For this we define a `Transaction` that encodes a monadic action, together with a description of that action and the possibility of needing human confirmation on the transaction.

``` {.haskell file=src/Transaction.hs}
{-# LANGUAGE NoImplicitPrelude,UndecidableInstances #-}
module Transaction where

import RIO
import qualified RIO.Text as T
<<transaction-imports>>

<<transaction>>
```

``` {.haskell #transaction}
data Description = Message Doc
                 | CreateFile FilePath
                 | WriteFile FilePath
                 | DeleteFile FilePath

data Transaction m = Transaction
  { action :: Maybe (m ())
  , description :: [Description]
  , needConfirm :: Bool }
```

Here `Doc` is pretty-printing class from `Console`.
Now, considering the different options for displaying a transaction, we need a human readable and a machine readable description

``` {.haskell #transaction}
showDescriptionHuman :: [Description] -> Doc
showDescriptionHuman = mconcat . map (\case
    Message x    -> x
    CreateFile f -> msgCreate f
    WriteFile f  -> msgWrite f
    DeleteFile f -> msgDelete f)

showDescriptionMachine :: [Description] -> Text
showDescriptionMachine = T.unlines . mapMaybe (\case
    Message _    -> Nothing
    CreateFile f -> Just $ "+ " <> T.pack f
    WriteFile f  -> Just $ "~ " <> T.pack f
    DeleteFile f -> Just $ "- " <> T.pack f)
```

When an event happened we need to respond, usually by writing out several files. Since `IO` is a `Monoid`, we can append `IO` actions and keep track of describing the gathered events in a `Transaction`. There are some things that we may need to ask the user permission for, like overwriting files in dubious circumstances. Messaging is done through pretty-printed `Doc`.

``` {.haskell #transaction-imports}
import Console (Doc, group, msgCreate, msgDelete, msgWrite)
import qualified Console
import qualified Data.Text.IO as T.IO
```

The `action` is wrapped in a `Maybe` so that we can tell if the `Transaction` does anything. A `Transaction` is a `Monoid`.

``` {.haskell #transaction}
instance (Semigroup (m ())) => Semigroup (Transaction m) where
    (Transaction al dl cl) <> (Transaction ar dr cr)
      = Transaction (al <> ar) (dl <> dr) (cl || cr)

instance (Monoid (m ())) => Monoid (Transaction m) where
    mempty = Transaction mempty mempty False
```

We can build `Transaction`s by appending elemental parts.

``` {.haskell #transaction}
plan :: (Monad m) => Description -> m () -> Transaction m
plan d action = Transaction (Just action) [d] False

doc :: Doc -> Transaction m
doc x = Transaction Nothing [Message x] False

confirm :: Transaction m
confirm = Transaction Nothing mempty True
```

In most of the program logic, `Transaction` will be available in terms of a `MonadWriter`.

``` {.haskell #transaction}
testTransaction :: (MonadIO m) => Transaction m -> m Bool
testTransaction (Transaction Nothing _ _)  = return False
testTransaction (Transaction (Just _) d _) = liftIO $ T.IO.putStr (showDescriptionMachine d) >> return True

runTransactionMachine :: (MonadIO m) => Transaction m -> m ()
runTransactionMachine (Transaction Nothing d _) = liftIO $ T.IO.putStr (showDescriptionMachine d)
runTransactionMachine (Transaction (Just x) d _) = do
    liftIO $ T.IO.putStr (showDescriptionMachine d)
    x

runTransaction :: (MonadIO m) => Maybe Doc -> Transaction m -> m ()
runTransaction (Just h) (Transaction Nothing d _) = liftIO $ Console.putTerminal $ group h (showDescriptionHuman d)
runTransaction Nothing (Transaction Nothing d _) = liftIO $ Console.putTerminal (showDescriptionHuman d)
runTransaction h (Transaction (Just x) d c) = do
    liftIO $ Console.putTerminal $ maybe (showDescriptionHuman d) (`group` showDescriptionHuman d) h
    if c then do
        reply <- liftIO $ do
            T.IO.putStr "confirm? (y/n) "
            hFlush stdout
            T.IO.getLine
        liftIO $ T.IO.putStrLn ""
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
    dump :: Text -> m ()

<<file-io-prim>>
<<file-io-instance>>
```

## Primitives

``` {.haskell #file-io-imports}
import qualified RIO.Text as T

import Errors (EntangledError(SystemError))
import Select (selectM)
```

### Make/Remove dir

``` {.haskell #file-io-imports}
import RIO.Directory ( createDirectoryIfMissing, doesDirectoryExist
                     , listDirectory, removeFile, removeDirectory )
import RIO.FilePath  ( (</>), splitDirectories, takeDirectory )
import RIO.List      ( scanl1 )
```

``` {.haskell #file-io-prim}
ensurePath :: (MonadIO m, MonadReader env m, HasLogFunc env)
           => FilePath -> m ()
ensurePath path = selectM (return ())
    [ ( not <$> doesDirectoryExist path
      , logDebug (display $ "creating directory `" <> T.pack path <> "`")
        >> createDirectoryIfMissing True path ) ]
```

``` {.haskell #file-io-prim}
rmDirIfEmpty :: (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env)
             => FilePath -> m ()
rmDirIfEmpty path = selectM (return ())
    [ ( not <$> doesDirectoryExist path
      , throwM $ SystemError $ "could not remove dir: `" <> T.pack path <> "`")
    , ( null <$> listDirectory path
      , logDebug (display $ "removing empty directory `" <> T.pack path <> "`")
        >> removeDirectory path ) ]

parents :: FilePath -> [FilePath]
parents = scanl1 (</>) . splitDirectories

rmPathIfEmpty :: (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env)
              => FilePath -> m ()
rmPathIfEmpty = mapM_ rmDirIfEmpty . reverse . parents
```

### Write file

``` {.haskell #file-io-imports}
import RIO.File ( writeBinaryFileDurable )
import qualified RIO.ByteString as B
```

``` {.haskell #file-io-prim}
writeIfChanged :: (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env)
               => FilePath -> Text -> m ()
writeIfChanged path text = do
    old_content' <- liftIO $ try $ B.readFile path
    case (old_content' :: Either IOException B.ByteString) of
        Right old_content | old_content == new_content -> return ()
                          | otherwise                  -> write
        Left  _                                        -> write
    where new_content = T.encodeUtf8 text
          write       = logDebug (display $ "writing `" <> T.pack path <> "`")
                      >> writeBinaryFileDurable path new_content

dump' :: (MonadIO m, MonadReader env m, HasLogFunc env)
      => Text -> m ()
dump' text = logDebug "dumping to stdio"
         >> B.hPutStr stdout (T.encodeUtf8 text)
```

## FileIO instance

``` {.haskell #file-io-instance}
newtype FileIO env a = FileIO { unFileIO :: RIO env a }
    deriving (Applicative, Functor, Semigroup, Monoid, Monad, MonadIO, MonadThrow, MonadReader env)

readFile' :: ( MonadIO m, HasLogFunc env, MonadReader env m, MonadThrow m )
          => FilePath -> m Text
readFile' path = decodeUtf8With lenientDecode
               <$> (logDebug (display $ "reading `" <> T.pack path <> "`")
                    >> B.readFile path)

runFileIO' :: ( MonadIO m, MonadReader env m, HasLogFunc env )
          => FileIO env a -> m a
runFileIO' (FileIO f) = do
    env <- ask
    runRIO env f

instance (HasLogFunc env) => MonadFileIO (FileIO env) where
    writeFile path text = ensurePath (takeDirectory path)
                        >> writeIfChanged path text

    deleteFile path     = logDebug (display $ "deleting `" <> T.pack path <> "`")
                        >> removeFile path
                        >> rmPathIfEmpty (takeDirectory path)

    readFile            = readFile'

    dump                = dump'
```

These are IO actions that need logging, possible confirmation by the user and execution. Also, using this we can do some mock testing.
