-- ------ language="Haskell" file="src/FileIO.hs" project://lit/a4-fileio.md
{-# LANGUAGE NoImplicitPrelude #-}
module FileIO where

import RIO
-- ------ begin <<file-io-imports>>[0] project://lit/a4-fileio.md
import RIO.Text (Text)
import qualified RIO.Text as T

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, throwM)

import Errors (EntangledError(SystemError))
import Select (selectM)
import TextUtil (tshow)
-- ------ end
-- ------ begin <<file-io-imports>>[1] project://lit/a4-fileio.md
import RIO.Directory ( createDirectoryIfMissing, doesDirectoryExist
                     , listDirectory, removeFile, removeDirectory )
import RIO.FilePath  ( (</>), splitDirectories )
import RIO.List      ( scanl1 )
-- ------ end
-- ------ begin <<file-io-imports>>[2] project://lit/a4-fileio.md
import RIO.File ( writeBinaryFileDurable )
import qualified RIO.ByteString as B
import Control.Exception ( IOException )
-- ------ end
-- ------ begin <<file-io-imports>>[3] project://lit/a4-fileio.md
import RIO.FilePath         ( takeDirectory )
import Control.Monad.Logger ( MonadLogger, MonadLoggerIO, LoggingT, logInfoN
                            , runStderrLoggingT )
-- ------ end

class Monad m => MonadFileIO m where
    writeFile :: FilePath -> Text -> m ()
    deleteFile :: FilePath -> m ()
    readFile :: FilePath -> m Text

-- ------ begin <<file-io-prim>>[0] project://lit/a4-fileio.md
ensurePath :: (MonadIO m, MonadLogger m) => FilePath -> m ()
ensurePath path = selectM (return ())
    [ ( not <$> doesDirectoryExist path
      , logInfoN ("creating directory `" <> (T.pack path) <> "`")
        >> createDirectoryIfMissing True path ) ]
-- ------ end
-- ------ begin <<file-io-prim>>[1] project://lit/a4-fileio.md
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
-- ------ end
-- ------ begin <<file-io-prim>>[2] project://lit/a4-fileio.md
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
-- ------ end
-- ------ begin <<file-io-instance>>[0] project://lit/a4-fileio.md
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
-- ------ end
-- ------ end
