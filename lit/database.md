# Database
We use an SQLite database to manage document content. Using SQL requires a remapping of the available data.

``` {.haskell file=src/Database.hs}
{-# LANGUAGE DeriveGeneric, OverloadedLabels #-}
module Database where

<<database-imports>>

newtype SQL a = SQL { unSQL :: ReaderT Connection IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow)

class (MonadIO m, MonadThrow m) => MonadSQL m where
    getConnection :: m Connection

instance MonadSQL SQL where
    getConnection = SQL ask

<<database-insertion>>
<<database-update>>
<<database-queries>>
```

``` {.haskell #database-imports}
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
<<import-text>>
<<import-map>>
import Data.Maybe (catMaybes)
import Data.Int (Int64)

import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch

import Document
import Config
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

``` {.sqlite file=schema.sql}
<<schema>>
```

::: {.TODO}
Implement the interface in Selda.
:::

## Insertion

### Documents

``` {.sqlite #schema}
create table if not exists "documents"
    ( "id"        integer primary key autoincrement
    , "filename"  text not null );
```

``` {.haskell #database-insertion}
liftSQL :: Connection -> SQL a -> IO a
liftSQL conn (SQL x) = runReaderT x conn

addDocument :: FilePath -> Document -> SQL ()
addDocument rel_path Document{..} = do
    conn <- getConnection
    liftIO $ execute conn "insert into `documents`(`filename`) values (?)" (Only rel_path)
    docId <- liftIO $ lastInsertRowId conn
    liftIO $ withTransaction conn $ liftSQL conn $ do
        insertCodes docId references
        insertContent docId documentContent
        insertTargets docId documentTargets
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

updateDocument :: FilePath -> Document -> SQL ()
updateDocument rel_path Document{..} = do
    conn <- getConnection
    docId' <- getDocumentId rel_path
    liftIO $ withTransaction conn $ liftSQL conn $ do
        docId <- case docId' of
            Just docId -> do
                removeDocumentData docId >> return docId
            Nothing    -> liftIO $ do
                execute conn "insert into `documents`(`filename`) values (?)" (Only rel_path)
                lastInsertRowId conn
        insertCodes docId references
        insertContent docId documentContent
        insertTargets docId documentTargets
```

### Stitch

::: {.TODO}
Make this update more efficient. See https://stackoverflow.com/questions/11563869/update-multiple-rows-with-different-values-in-a-single-sql-query for an example.
:::

``` {.haskell #database-update}
updateTarget :: [ReferencePair] -> SQL () 
updateTarget refs = do
    conn <- getConnection
    let update (ReferenceId (ReferenceName name) count, CodeBlock{codeSource})
            = execute conn "update `codes` set `source` = ? where `name` is ? and `ordinal` is ?"
                  (codeSource, name, count)
    liftIO $ mapM_ update refs
```

## Queries

``` {.haskell #database-queries}
listTargetFiles :: SQL [FilePath]
listTargetFiles = do
    conn <- getConnection
    map fromOnly <$>
        liftIO (query_ conn "select (`filename`) from `targets`" :: IO [Only FilePath])
```

``` {.haskell #database-queries}
listSourceFiles :: SQL [FilePath]
listSourceFiles = do
    conn <- getConnection
    map fromOnly <$>
        liftIO (query_ conn "select (`filename`) from `documents`" :: IO [Only FilePath])
```

### References

``` {.haskell #database-queries}
queryReferenceMap :: ( MonadReader Config m
                     , MonadSQL m )
                  => m ReferenceMap
queryReferenceMap = do
        conn <- getConnection
        config <- ask
        rows <- liftIO (query_ conn "select (`name`, `ordinal`, `source`, `language`) from `codes`" :: IO [(Text, Int, Text, Text)])
        M.fromList <$> mapM (refpair config) rows
    where refpair config (name, ordinal, source, lang) =
            case (languageFromName config lang) of
                Nothing -> throwM $ DatabaseError $ "unknown language: " <> lang
                Just l  -> return ( ReferenceId (ReferenceName name) ordinal
                                  , CodeBlock (KnownLanguage l) [] source )
```

### Document

