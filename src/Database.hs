-- ------ language="Haskell" file="src/Database.hs" project://lit/03-database.md#5
{-# LANGUAGE DeriveGeneric, OverloadedLabels #-}
module Database where

-- ------ begin <<database-imports>>[0] project://lit/03-database.md#21
import Paths_entangled

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Writer
import Control.Monad.Logger

-- ------ begin <<import-text>>[0] project://lit/01-entangled.md#44
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import qualified Data.Text.IO as T.IO
-- ------ end
-- ------ begin <<database-imports>>[1] project://lit/03-database.md#84
-- ------ begin <<import-map>>[0] project://lit/01-entangled.md#24
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
-- ------ end
import Data.Maybe (catMaybes)
import Data.Int (Int64)

import Document
import Config
import TextUtil
-- ------ end
-- ------ begin <<database-types>>[0] project://lit/03-database.md#37
newtype SQL a = SQL { unSQL :: ReaderT Connection (LoggingT IO) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadLogger, MonadLoggerIO)

class (MonadIO m, MonadThrow m, MonadLogger m) => MonadSQL m where
    getConnection :: m Connection
    runSQL :: (MonadIO n, MonadLoggerIO n) => Connection -> m a -> n a

-- type LogLine = (Loc, LogSource, LogLevel, LogStr)

instance MonadSQL SQL where
    getConnection = SQL ask
    runSQL conn (SQL x) = do
        logFun <- askLoggerIO
        x <- liftIO $ runLoggingT (runReaderT x conn) logFun
        return x

withSQL :: (MonadIO m, MonadLoggerIO m) => FilePath -> SQL a -> m a
withSQL p (SQL x) = do
    logFun <- askLoggerIO
    x <- liftIO $ withConnection p (\conn -> liftIO $ runLoggingT (runReaderT x conn) logFun)
    return x

expectUnique :: (MonadThrow m, Show a) => [a] -> m (Maybe a)
expectUnique []  = return Nothing
expectUnique [x] = return $ Just x
expectUnique lst = throwM $ DatabaseError $ "duplicate entry: " <> tshow lst

expectUnique' :: (MonadThrow m, Show a) => (a -> b) -> [a] -> m (Maybe b)
expectUnique' _ []  = return Nothing
expectUnique' f [x] = return $ Just (f x)
expectUnique' _ lst = throwM $ DatabaseError $ "duplicate entry: " <> tshow lst
-- ------ end
-- ------ begin <<database-types>>[1] project://lit/03-database.md#75
withTransactionM :: (MonadSQL m, MonadLoggerIO m) => m a -> m a
withTransactionM t = do
    conn <- getConnection
    logFun <- askLoggerIO
    x <- liftIO $ withTransaction conn (runLoggingT (runSQL conn t) logFun)
    return x
-- ------ end
-- ------ begin <<database-create>>[0] project://lit/03-database.md#128
schema :: IO [Query]
schema = do
    schema_path <- getDataFileName "data/schema.sql"
    qs <-  T.splitOn ";" <$> T.IO.readFile schema_path
    return $ map Query (init qs)

createTables :: SQL ()
createTables = do
    conn <- getConnection
    liftIO $ schema >>= mapM_ (execute_ conn)
-- ------ end
-- ------ begin <<database-insertion>>[0] project://lit/03-database.md#170
insertCode :: Int64 -> ReferencePair -> SQL ()
insertCode docId ( ReferenceId _ (ReferenceName name) count
                 , CodeBlock (KnownLanguage langName) attrs source ) = do
    conn <- getConnection
    liftIO $ do
        execute conn "insert into `codes`(`name`,`ordinal`,`source`,`language`,`document`) \
                     \ values (?,?,?,?,?)" (name, count, source, langName, docId)
        codeId <- lastInsertRowId conn
        executeMany conn "insert into `classes` values (?,?)"
                    [(className, codeId) | className <- getClasses attrs]
    where getClasses [] = []
          getClasses (CodeClass c : cs) = c : getClasses cs
          getClasses (_ : cs) = getClasses cs

insertCodes :: Int64 -> ReferenceMap -> SQL ()
insertCodes docId codes = mapM_ (insertCode docId) (M.toList codes)
-- ------ end
-- ------ begin <<database-insertion>>[1] project://lit/03-database.md#227
queryCodeId' :: Int64 -> ReferenceId -> SQL (Maybe Int64)
queryCodeId' docId (ReferenceId _ (ReferenceName name) count) = do
    conn <- getConnection
    expectUnique' fromOnly =<< (liftIO $ query conn codeQuery (docId, name, count))
    where codeQuery = "select `id` from `codes` where \
                      \ `document` is ? and `name` is ? and `ordinal` is ?"

queryCodeId :: ReferenceId -> SQL (Maybe Int64)
queryCodeId (ReferenceId doc (ReferenceName name) count) = do
    conn    <- getConnection
    expectUnique' fromOnly =<< (liftIO $ query conn codeQuery (doc, name, count))
    where codeQuery = "select `codes`.`id` \
                      \ from `codes` inner join `documents` \
                      \     on `documents`.`id` is `codes`.`document` \
                      \ where   `documents`.`filename` is ? \
                      \     and `name` is ? \
                      \     and `ordinal` is ?"

queryCodeSource :: ReferenceId -> SQL (Maybe Text)
queryCodeSource (ReferenceId doc (ReferenceName name) count) = do
    conn    <- getConnection
    expectUnique' fromOnly =<< (liftIO $ query conn codeQuery (doc, name, count))
    where codeQuery = "select `codes`.`source` \
                      \ from `codes` inner join `documents` \
                      \     on `documents`.`id` is `codes`.`document` \
                      \ where   `documents`.`filename` is ? \
                      \     and `name` is ? \
                      \     and `ordinal` is ?"

contentToRow :: Int64 -> Content -> SQL (Int64, Maybe Text, Maybe Int64)
contentToRow docId (PlainText text) = return (docId, Just text, Nothing)
contentToRow docId (Reference ref)  = do
    codeId' <- queryCodeId' docId ref
    case codeId' of
        Nothing     -> throwM $ DatabaseError $ "expected reference: " <> tshow ref
        Just codeId -> return (docId, Nothing, Just codeId)

insertContent :: Int64 -> [Content] -> SQL ()
insertContent docId content = do
    rows <- mapM (contentToRow docId) content
    conn <- getConnection
    liftIO $ executeMany conn "insert into `content`(`document`,`plain`,`code`) values (?,?,?)" rows
-- ------ end
-- ------ begin <<database-insertion>>[2] project://lit/03-database.md#286
insertTargets :: Int64 -> FileMap -> SQL ()
insertTargets docId files = do
        conn <- getConnection
        -- liftIO $ print rows
        liftIO $ executeMany conn "insert into `targets`(`filename`,`codename`,`language`,`document`) values (?, ?, ?, ?)" rows
    where targetRow (path, (ReferenceName name, lang)) = (path, name, lang, docId)
          rows = map targetRow (M.toList files)
-- ------ end
-- ------ begin <<database-update>>[0] project://lit/03-database.md#302
getDocumentId :: FilePath -> SQL (Maybe Int64)
getDocumentId rel_path = do
        conn <- getConnection
        expectUnique' fromOnly =<< (liftIO $ query conn documentQuery (Only rel_path))
    where documentQuery = "select `id` from `documents` where `filename` is ?"

removeDocumentData :: Int64 -> SQL ()
removeDocumentData docId = do
    conn <- getConnection
    liftIO $ do
        execute conn "delete from `targets` where `document` is ?" (Only docId)
        execute conn "delete from `content` where `document` is ?" (Only docId)
        execute conn "delete from `codes` where `document` is ?" (Only docId)

insertDocument :: FilePath -> Document -> SQL ()
insertDocument rel_path Document{..} = do
    conn <- getConnection
    docId' <- getDocumentId rel_path
    docId <- case docId' of
        Just docId -> do
            logInfoN $ "Replacing '" <> T.pack rel_path <> "'."
            removeDocumentData docId >> return docId
        Nothing    -> do
            logInfoN $ "Inserting new '" <> T.pack rel_path <> "'."
            liftIO $ execute conn "insert into `documents`(`filename`) values (?)" (Only rel_path)
            liftIO $ lastInsertRowId conn
    insertCodes docId references
    insertContent docId documentContent
    insertTargets docId documentTargets
-- ------ end
-- ------ begin <<database-update>>[1] project://lit/03-database.md#341
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM whenNothing x = do
    unboxed <- x
    case unboxed of
        Nothing -> whenNothing
        Just x' -> return x'

updateCode :: ReferencePair -> SQL ()
updateCode (ref, CodeBlock {codeSource}) = do
    codeId <- fromMaybeM (throwM $ DatabaseError $ "could not find code '" <> tshow ref <> "'")
                         (queryCodeId ref)
    conn   <- getConnection
    liftIO $ execute conn "update `codes` set `source` = ? where `id` is ?" (codeSource, codeId)

updateTarget :: [ReferencePair] -> SQL () 
updateTarget refs = withTransactionM $ mapM_ updateCode refs
-- ------ end
-- ------ begin <<database-queries>>[0] project://lit/03-database.md#362
listTargetFiles :: SQL [FilePath]
listTargetFiles = do
    conn <- getConnection
    map fromOnly <$>
        liftIO (query_ conn "select `filename` from `targets`" :: IO [Only FilePath])
-- ------ end
-- ------ begin <<database-queries>>[1] project://lit/03-database.md#370
listSourceFiles :: SQL [FilePath]
listSourceFiles = do
    conn <- getConnection
    map fromOnly <$>
        liftIO (query_ conn "select `filename` from `documents`" :: IO [Only FilePath])
-- ------ end
-- ------ begin <<database-queries>>[2] project://lit/03-database.md#378
queryTargetRef :: FilePath -> SQL (Maybe (ReferenceName, Text))
queryTargetRef rel_path = do
    conn <- getConnection
    val  <- expectUnique =<< (liftIO $ query conn targetQuery (Only rel_path))
    return $ case val of
        Nothing           -> Nothing
        Just (name, lang) -> Just (ReferenceName name, lang)
    where targetQuery = "select `codename`,`language` from `targets` where `filename` is ?" 
-- ------ end
-- ------ begin <<database-queries>>[3] project://lit/03-database.md#389
stitchDocument :: FilePath -> SQL Text
stitchDocument rel_path = do
    conn <- getConnection
    docId <- getDocumentId rel_path >>= \case
        Nothing -> throwM $ StitchError $ "File `" <> T.pack rel_path <> "` not in database."
        Just x  -> return x
    result <- liftIO (query conn "select coalesce(`plain`,`codes`.`source`) from `content` \
                                 \  left outer join `codes` \
                                 \    on `code` is `codes`.`id` \
                                 \  where `content`.`document` is ?" (Only docId) :: IO [Only Text])
    return $ unlines' $ map fromOnly result
-- ------ end
-- ------ begin <<database-queries>>[4] project://lit/03-database.md#405
queryReferenceMap :: Config -> SQL ReferenceMap
queryReferenceMap config = do
        conn <- getConnection
        rows <- liftIO (query_ conn "select `documents`.`filename`, `name`, `ordinal`, `source`, `language` from `codes` inner join `documents` on `codes`.`document` is `documents`.`id`" :: IO [(FilePath, Text, Int, Text, Text)])
        M.fromList <$> mapM (refpair config) rows
    where refpair config (rel_path, name, ordinal, source, lang) =
            case (languageFromName config lang) of
                Nothing -> throwM $ DatabaseError $ "unknown language: " <> lang
                Just l  -> return ( ReferenceId rel_path (ReferenceName name) ordinal
                                  , CodeBlock (KnownLanguage lang) [] source )
-- ------ end
-- ------ end
