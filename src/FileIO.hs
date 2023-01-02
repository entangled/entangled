-- ~\~ language=Haskell filename=src/FileIO.hs
-- ~\~ begin <<lit/a4-fileio.md|src/FileIO.hs>>[init]
{-# LANGUAGE NoImplicitPrelude #-}
module FileIO where

import RIO
-- ~\~ begin <<lit/a4-fileio.md|file-io-imports>>[init]
import qualified RIO.Text as T

import Errors (EntangledError(SystemError))
import Select (selectM)
-- ~\~ end
-- ~\~ begin <<lit/a4-fileio.md|file-io-imports>>[1]
import RIO.Directory ( createDirectoryIfMissing, doesDirectoryExist
                     , listDirectory, removeFile, removeDirectory )
import RIO.FilePath  ( (</>), splitDirectories, takeDirectory )
import RIO.List      ( scanl1 )
-- ~\~ end
-- ~\~ begin <<lit/a4-fileio.md|file-io-imports>>[2]
import RIO.File ( writeBinaryFileDurable )
import qualified RIO.ByteString as B
-- ~\~ end

class Monad m => MonadFileIO m where
    writeFile :: FilePath -> Text -> m ()
    deleteFile :: FilePath -> m ()
    readFile :: FilePath -> m Text
    dump :: Text -> m ()

-- ~\~ begin <<lit/a4-fileio.md|file-io-prim>>[init]
ensurePath :: (MonadIO m, MonadReader env m, HasLogFunc env)
           => FilePath -> m ()
ensurePath path = selectM (return ())
    [ ( not <$> doesDirectoryExist path
      , logDebug (display $ "creating directory `" <> T.pack path <> "`")
        >> createDirectoryIfMissing True path ) ]
-- ~\~ end
-- ~\~ begin <<lit/a4-fileio.md|file-io-prim>>[1]
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
-- ~\~ end
-- ~\~ begin <<lit/a4-fileio.md|file-io-prim>>[2]
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
-- ~\~ end
-- ~\~ begin <<lit/a4-fileio.md|file-io-instance>>[init]
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
-- ~\~ end
-- ~\~ end
