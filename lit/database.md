# Database
We use an SQLite database to manage document content. Using SQL requires a remapping of the available data.

``` {.haskell file=src/Database.hs}
{-# LANGUAGE DeriveGeneric, OverloadedLabels #-}
module Database where

<<database-imports>>
<<database-types>>
<<database-create>>
<<database-insertion>>
<<database-update>>
<<database-queries>>
```

## Types

We wrap all SQL interaction in a `SQL` monad, which stores the `Connection` object and a logger.

``` {.haskell #database-imports}
import Paths_entangled

import Logging

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Writer

<<import-text>>
import qualified Data.Text.IO as T.IO
```

``` {.haskell #database-types}
newtype SQL a = SQL { unSQL :: WriterT [(LogLevel, Text)] (ReaderT Connection IO) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadLogger)

class (MonadIO m, MonadThrow m, MonadLogger m) => MonadSQL m where
    getConnection :: m Connection
    runSQL :: (MonadIO n, MonadLogger n) => Connection -> m a -> n a

instance MonadSQL SQL where
    getConnection = SQL ask
    runSQL conn (SQL x) = do
        (x, msgs) <- liftIO $ runReaderT (runWriterT x) conn
        forwardEntries msgs
        return x

withSQL :: (MonadIO m, MonadLogger m) => FilePath -> SQL a -> m a
withSQL p (SQL x) = do
    (x, msgs) <- liftIO $ withConnection p (liftIO . runReaderT (runWriterT x))
    forwardEntries msgs
    return x
```

The `SQLite.Simple` function `withTransaction` takes an `IO` action as argument. We somehow have to redirect logging information around the unpacking to `IO` and lifting back to `MonadSQL`. This is not the prettiest solution, and we see some repetition of the pattern where we unpack result and log, forward log to outer monad and return result pattern.

All the `RedirectLog` code is needed to aid the type checker, or it won't know what to do.

``` {.haskell #database-types}
type RedirectLog m a = WriterT [(LogLevel, Text)] m a

redirectLogger :: (MonadIO m, MonadSQL n) => Connection -> n a -> RedirectLog m a
redirectLogger conn x = runSQL conn x

withTransactionM :: (MonadSQL m) => m a -> m a
withTransactionM t = do
    conn <- getConnection
    (x, msgs) <- liftIO $ withTransaction conn $ runWriterT $ redirectLogger conn t
    forwardEntries msgs
    return x
```

``` {.haskell #database-imports}
<<import-map>>
import Data.Maybe (catMaybes)
import Data.Int (Int64)

import Document
import Config
import TextUtil
```

::: {.note}
use `pragma foreign_keys = true`.
:::

A diagram for this schema, generated with `sqleton`. It would be awesome if the following would work.

``` {.make #-knit- .-hidden-}
schema.svg: <<file|schema>>
    cat $< | sqlite3 database
    sqleton -o $@ database -e -L circo 
```

![Schema](schema.svg)

In `SQLite.Simple` the above schema becomes

``` {.sqlite file=data/schema.sql}
pragma synchronous = off;
pragma journal_mode = memory;

<<schema>>
```

::: {.TODO}
Implement the interface in Selda.
:::

## Create database

``` {.haskell #database-create}
schema :: IO [Query]
schema = do
    schema_path <- getDataFileName "data/schema.sql"
    qs <-  T.splitOn ";" <$> T.IO.readFile schema_path
    return $ map Query (init qs)

createTables :: SQL ()
createTables = do
    conn <- getConnection
    liftIO $ schema >>= mapM_ (execute_ conn)
```

## Insertion

### Documents

``` {.sqlite #schema}
create table if not exists "documents"
    ( "id"        integer primary key autoincrement
    , "filename"  text not null );
```

### Codes

The code table maps to a `ReferencePair` containing both `CodeBlock` and `ReferenceId`. I use the `name`, `ordinal` pair as primary key.

``` {.sqlite #schema}
create table if not exists "codes"
    ( "name"      text not null
    , "ordinal"   integer not null
    , "source"    text not null
    , "language"  text not null
    , "document"  integer not null
    , primary key ("name", "ordinal")
    , foreign key ("document") references "documents"("id") );
```

``` {.haskell #database-insertion}
insertCodes :: Int64 -> ReferenceMap -> SQL ()
insertCodes docId codes = do
        conn <- getConnection
        liftIO $ executeMany conn "insert into `codes` values (?,?,?,?,?)" rows
    where codeRow ( (ReferenceId (ReferenceName name) count)
                  , (CodeBlock (KnownLanguage Language{languageName}) _ source) )
              = Just (name, count, source, languageName, docId)
          codeRow _
              = Nothing
          rows = catMaybes $ map codeRow (M.toList codes)
```

### Content

``` {.sqlite #schema}
create table if not exists "content"
    ( "id"          integer primary key autoincrement
    , "document"    integer not null
    , "plain"       text
    , "codeName"    text
    , "codeOrdinal" integer
    , foreign key ("document") references "documents"("id")
    , foreign key ("codeName") references "codes"("name")
    , foreign key ("codeOrdinal") references "codes"("ordinal")
    , check ("plain" is not null or ("codeName" is not null and "codeOrdinal" is not null)) );
```

``` {.haskell #database-insertion}
insertContent :: Int64 -> [Content] -> SQL ()
insertContent docId content = do
        conn <- getConnection
        liftIO $ executeMany conn "insert into `content`(`document`,`plain`,`codeName`,`codeOrdinal`) values (?,?,?,?)" rows
    where contentRow (PlainText text)
              = (docId, Just text, Nothing, Nothing)
          contentRow (Reference (ReferenceId (ReferenceName name) count))
              = (docId, Nothing, Just name, Just count)
          rows = map contentRow content
```

### Targets

``` {.sqlite #schema}
create table if not exists "targets"
    ( "filename"  text not null unique
    , "codename"  text not null
    , "document"  integer not null
    , foreign key ("codename") references "codes"("name")
    , foreign key ("document") references "documents"("id") );
```

``` {.haskell #database-insertion}
insertTargets :: Int64 -> Map FilePath ReferenceName -> SQL ()
insertTargets docId files = do
        conn <- getConnection
        liftIO $ executeMany conn "insert into `targets` values (?, ?, ?)" rows
    where targetRow (path, ReferenceName name) = (path, name, docId)
          rows = map targetRow (M.toList files)
```

## Updating

### Tangle

When a Markdown source is written to, we first remove all data that was inserted from that document and then reinsert the new data. This is to prevent that code blocks remain from older versions. This problem could also be solved by a separate garbage collection step.

``` {.haskell #database-update}
getDocumentId :: FilePath -> SQL (Maybe Int64)
getDocumentId rel_path = do
    conn <- getConnection
    docId' <- liftIO $ query conn "select `id` from `documents` where `filename` is ?" (Only rel_path)
    case docId' of
        []             -> return Nothing
        [(Only docId)] -> return $ Just docId
        _              -> throwM $ DatabaseError
                                 $ "file `" <> T.pack rel_path <> "` has multiple entries."

removeDocumentData :: Int64 -> SQL ()
removeDocumentData docId = do
    conn <- getConnection
    liftIO $ do
        execute conn "delete from `content` where `document` is ?" (Only docId)
        execute conn "delete from `codes` where `document` is ?" (Only docId)
        execute conn "delete from `targets` where `document` is ?" (Only docId)

insertDocument :: FilePath -> Document -> SQL ()
insertDocument rel_path Document{..} = do
    conn <- getConnection
    docId' <- getDocumentId rel_path
    withTransactionM $ do
        docId <- case docId' of
            Just docId -> do
                logMessage $ "Replacing '" <> T.pack rel_path <> "'."
                removeDocumentData docId >> return docId
            Nothing    -> do
                logMessage $ "Inserting new '" <> T.pack rel_path <> "'."
                liftIO $ execute conn "insert into `documents`(`filename`) values (?)" (Only rel_path)
                liftIO $ lastInsertRowId conn
        insertCodes docId references
        insertContent docId documentContent
        insertTargets docId documentTargets
```

### Stitch

::: {.TODO}
Make this update more efficient. See https://stackoverflow.com/questions/11563869/update-multiple-rows-with-different-values-in-a-single-sql-query for an example.
Other performance related post: https://stackoverflow.com/questions/1711631/improve-insert-per-second-performance-of-sqlite
:::

``` {.haskell #database-update}
updateTarget :: [ReferencePair] -> SQL () 
updateTarget refs = withTransactionM $ mapM_ update refs
    where update (ReferenceId (ReferenceName name) count, CodeBlock{codeSource}) = do
              conn <- getConnection
              liftIO $ execute conn "update `codes` set `source` = ? where `name` is ? and `ordinal` is ?"
                               (codeSource, name, count)
```

## Queries

``` {.haskell #database-queries}
listTargetFiles :: SQL [FilePath]
listTargetFiles = do
    conn <- getConnection
    map fromOnly <$>
        liftIO (query_ conn "select `filename` from `targets`" :: IO [Only FilePath])
```

``` {.haskell #database-queries}
listSourceFiles :: SQL [FilePath]
listSourceFiles = do
    conn <- getConnection
    map fromOnly <$>
        liftIO (query_ conn "select `filename` from `documents`" :: IO [Only FilePath])
```

``` {.haskell #database-queries}
queryTargetRef :: FilePath -> SQL (Maybe ReferenceName)
queryTargetRef rel_path = do
    conn <- getConnection
    x' <- liftIO (query conn "select `codename` from `targets` where `filename` is ?" (Only rel_path) :: IO [Only Text])
    case x' of
        []  -> return Nothing
        [Only x] -> return $ Just (ReferenceName x)
        _   -> throwM $ DatabaseError $ "target file `" <> T.pack rel_path <> "` has multiple entries."
```

``` {.haskell #database-queries}
stitchDocument :: FilePath -> SQL Text
stitchDocument rel_path = do
    conn <- getConnection
    docId <- getDocumentId rel_path >>= \case
        Nothing -> throwM $ StitchError $ "File `" <> T.pack rel_path <> "` not in database."
        Just x  -> return x
    result <- liftIO (query conn "select coalesce(`plain`,`codes`.`source`) from `content` \
                                 \  left outer join `codes` \
                                 \    on (`codeName`,`codeOrdinal`)=(`codes`.`name`,`codes`.`ordinal`) \
                                 \  where `content`.`document` is ?" (Only docId) :: IO [Only Text])
    return $ unlines' $ map fromOnly result
```

### References

``` {.haskell #database-queries}
queryReferenceMap :: Config -> SQL ReferenceMap
queryReferenceMap config = do
        conn <- getConnection
        rows <- liftIO (query_ conn "select `name`, `ordinal`, `source`, `language` from `codes`" :: IO [(Text, Int, Text, Text)])
        M.fromList <$> mapM (refpair config) rows
    where refpair config (name, ordinal, source, lang) =
            case (languageFromName config lang) of
                Nothing -> throwM $ DatabaseError $ "unknown language: " <> lang
                Just l  -> return ( ReferenceId (ReferenceName name) ordinal
                                  , CodeBlock (KnownLanguage l) [] source )
```

### Document

