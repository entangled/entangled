# Database
We use an SQLite database to manage document content. Using SQL requires a remapping of the available data.

``` {.haskell file=src/Database.hs}
{-# LANGUAGE OverloadedLabels, NoImplicitPrelude #-}
module Database where

import RIO
import RIO.List (initMaybe, sortOn)
import qualified RIO.Text as T
import qualified RIO.Map as M

<<database-imports>>
<<database-types>>
<<database-create>>
<<database-insertion>>
<<database-update>>
<<database-queries>>

deduplicateRefs :: [ReferencePair] -> SQL [ReferencePair]
deduplicateRefs refs = dedup $ sortOn fst refs
    where dedup [] = return []
          dedup [x1] = return [x1]
          dedup ((ref1, code1@CodeBlock{codeSource=s1}) : (ref2, code2@CodeBlock{codeSource=s2}) : xs)
                | ref1 /= ref2 = ((ref1, code1) :) <$> dedup ((ref2, code2) : xs)
                | s1 == s2     = dedup ((ref1, code1) : xs)
                | otherwise    = do
                    old_code <- queryCodeSource ref1
                    case old_code of
                        Nothing -> throwM $ StitchError $ "ambiguous update: " <> tshow ref1 <> " not in database."
                        Just c  -> select (throwM $ StitchError $ "ambiguous update to " <> tshow ref1)
                                    [(s1 == c && s2 /= c, dedup ((ref2, code2) : xs))
                                    ,(s1 /= c && s2 == c, dedup ((ref1, code1) : xs))]
```

## Types

We wrap all SQL interaction in a `SQL` monad, which stores the `Connection` object and a logger.

``` {.haskell #database-imports}
import Paths_entangled

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow ()

import qualified Data.Text.IO as T.IO
```

``` {.haskell #database-types}
class HasConnection env where
    connection :: Lens' env Connection

data SQLEnv = SQLEnv
    { sqlLogFunc    :: LogFunc
    , sqlConnection :: Connection }

newtype SQL a = SQL { unSQL :: RIO SQLEnv a }  -- ReaderT Connection (LoggingT IO) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadReader SQLEnv)

instance HasLogFunc SQLEnv where
    logFuncL = lens sqlLogFunc (\x y -> x { sqlLogFunc = y })

instance HasConnection SQLEnv where
    connection = lens sqlConnection (\x y -> x { sqlConnection = y })

getConnection :: (HasConnection env, MonadReader env m) => m Connection
getConnection = view connection

db :: (MonadIO m, MonadReader env m, HasConnection env, HasLogFunc env)
   => SQL a -> m a
db (SQL x) = do
    logFunc <- view logFuncL
    conn    <- view connection
    runRIO (SQLEnv logFunc conn) x

runSQL' :: (MonadIO m) => SQLEnv -> SQL a -> m a
runSQL' env (SQL x) = runRIO env x

expectUnique :: (MonadThrow m, Show a) => [a] -> m (Maybe a)
expectUnique []  = return Nothing
expectUnique [x] = return $ Just x
expectUnique lst = throwM $ DatabaseError $ "duplicate entry: " <> tshow lst

expectUnique' :: (MonadThrow m, Show a) => (a -> b) -> [a] -> m (Maybe b)
expectUnique' _ []  = return Nothing
expectUnique' f [x] = return $ Just (f x)
expectUnique' _ lst = throwM $ DatabaseError $ "duplicate entry: " <> tshow lst
```

The `SQLite.Simple` function `withTransaction` takes an `IO` action as argument. We somehow have to redirect logging information around the unpacking to `IO` and lifting back to `MonadSQL`. This is not the prettiest solution, and we see some repetition of the pattern where we unpack result and log, forward log to outer monad and return result pattern.

All the `RedirectLog` code is needed to aid the type checker, or it won't know what to do.

``` {.haskell #database-types}
withTransactionM :: SQL a -> SQL a
withTransactionM t = do
    env <- ask
    conn <- getConnection
    liftIO $ withTransaction conn (runSQL' env t)
```

``` {.haskell #database-imports}
import Document
import Config
import Select (select)
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
-- rules when editing this schema:
-- * always use double quotes for identifiers,
-- * align types for easy reading
pragma synchronous = off;
pragma journal_mode = memory;
pragma foreign_keys = on;

<<schema>>
-- vim:ft=mysql
```

::: {.TODO}
Implement the interface in Selda.
:::

## Create database

``` {.haskell #database-create}
schema :: IO [Query]
schema = do
    schema_path <- getDataFileName "data/schema.sql"
    qs <- initMaybe . T.split (== ';') <$> T.IO.readFile schema_path
    return $ maybe [] (map Query) qs

createTables :: (MonadIO m, MonadReader env m, HasConnection env) => m ()
createTables = do
    conn <- getConnection
    liftIO $ schema >>= mapM_ (execute_ conn)
```

## Insertion

### Documents

``` {.sqlite #schema}
-- this table should be sorted on order of inclusion
create table if not exists "documents"
    ( "id"        integer primary key autoincrement
    , "filename"  text not null
    , "time"      timestamp default current_timestamp not null
    );
```

### Codes

The code table maps to a `ReferencePair` containing both `CodeBlock` and `ReferenceId`. I use the `name`, `ordinal` pair as primary key.

``` {.sqlite #schema}
create table if not exists "codes"
    ( "id"         integer primary key autoincrement
    , "name"       text not null
    , "ordinal"    integer not null
    , "source"     text not null
    , "language"   text not null
    , "document"   integer not null
    , "linenumber" integer
    , foreign key ("document") references "documents"("id")
    );
```

``` {.haskell #database-insertion}
insertCode' :: (Text, Int, Text, Maybe Text, Int64, Maybe Int) -> [CodeProperty] -> SQL ()
insertCode' tup attrs =  do
    conn <- getConnection
    liftIO $ do
        execute conn "insert into `codes`(`name`,`ordinal`,`source`,`language`,`document`,`linenumber`) \
                     \ values (?,?,?,?,?,?)" tup
        codeId <- lastInsertRowId conn
        executeMany conn "insert into `classes` values (?,?)"
                    [(className, codeId) | className <- getClasses attrs]
    where getClasses [] = []
          getClasses (CodeClass c : cs) = c : getClasses cs
          getClasses (_ : cs) = getClasses cs

insertCode :: Int64 -> ReferencePair -> SQL ()
insertCode docId ( ReferenceId file (ReferenceName name) count
                 , CodeBlock lang attrs source linenum ) = do
    langName <- case lang of
        UnknownClass c  -> logWarn (display $ "unknown language `" <> c <> "` in "
                                 <> T.pack file <> ":<<" <> name <> ">>")
                        >> return (Just c)
        NoLanguage      -> logWarn (display $ "no language class in "
                                 <> T.pack file <> ":<<" <> name <> ">>")
                        >> return Nothing
        KnownLanguage l -> return (Just l)
    insertCode' (name, count, source, langName, docId, linenum) attrs

insertCodes :: Int64 -> ReferenceMap -> SQL ()
insertCodes docId codes = mapM_ (insertCode docId) (M.toList codes)
```

A table that references specific code blocks should reference both the code name and the code ordinal.

``` {.sqlite #reference-code}
, "code"        text not null
, foreign key ("code") references "codes"("id") on delete cascade
```

### Classes and attributes

``` {.sqlite #schema}
create table if not exists "classes"
    ( "class"     text not null
    <<reference-code>>
    );

create table if not exists "attributes"
    ( "attribute"   text not null
    , "value"       text not null
    <<reference-code>>
    );
```



### Content

``` {.sqlite #schema}
create table if not exists "content"
    ( "id"          integer primary key autoincrement
    , "document"    integer not null
    , "plain"       text
    , "code"        integer
    , foreign key ("document") references "documents"("id")
    , foreign key ("code") references "codes"("id")
    );
    -- , check ("plain" is not null or ("codeName" is not null and "codeOrdinal" is not null)) )
```

``` {.haskell #database-insertion}
queryCodeId' :: Int64 -> ReferenceId -> SQL (Maybe Int64)
queryCodeId' docId (ReferenceId _ (ReferenceName name) count) = do
    conn <- getConnection
    expectUnique' fromOnly =<< liftIO (query conn codeQuery (docId, name, count))
    where codeQuery = "select `id` from `codes` where \
                      \ `document` is ? and `name` is ? and `ordinal` is ?"

queryCodeId :: ReferenceId -> SQL (Maybe Int64)
queryCodeId (ReferenceId doc (ReferenceName name) count) = do
    conn    <- getConnection
    expectUnique' fromOnly =<< liftIO (query conn codeQuery (doc, name, count))
    where codeQuery = "select `codes`.`id` \
                      \ from `codes` inner join `documents` \
                      \     on `documents`.`id` is `codes`.`document` \
                      \ where   `documents`.`filename` is ? \
                      \     and `name` is ? \
                      \     and `ordinal` is ?"

queryCodeSource :: ReferenceId -> SQL (Maybe Text)
queryCodeSource (ReferenceId doc (ReferenceName name) count) = do
    conn    <- getConnection
    expectUnique' fromOnly =<< liftIO (query conn codeQuery (doc, name, count))
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
```

### Targets

``` {.sqlite #schema}
create table if not exists "targets"
    ( "filename"  text primary key
    , "codename"  text not null
    , "language"  text not null
    , "document"  integer          -- in case this is null, the target is orphaned
    , "time"      timestamp default current_timestamp not null
    -- , foreign key ("codename") references "codes"("name")
    , foreign key ("document") references "documents"("id")
    );
```

``` {.haskell #database-insertion}
insertTargets :: Int64 -> FileMap -> SQL ()
insertTargets docId files = do
        conn <- getConnection
        -- liftIO $ print rows
        liftIO $ executeMany conn "replace into `targets`(`filename`,`codename`,`language`,`document`) values (?, ?, ?, ?)" rows
    where targetRow (path, (ReferenceName name, lang)) = (path, name, lang, docId)
          rows = map targetRow (M.toList files)
```

## Updating

### Tangle

When a Markdown source is written to, we first remove all data that was inserted from that document and then reinsert the new data. This is to prevent that code blocks remain from older versions. This problem could also be solved by a separate garbage collection step.

``` {.haskell #database-update}
getDocumentId :: FilePath -> SQL (Maybe Int64)
getDocumentId rel_path = do
        conn <- getConnection
        expectUnique' fromOnly =<< liftIO (query conn documentQuery (Only rel_path))
    where documentQuery = "select `id` from `documents` where `filename` is ?"

orphanTargets :: Int64 -> SQL ()
orphanTargets docId = do
    conn <- getConnection
    liftIO $ execute conn "update `targets` set `document` = null where `document` is ?" (Only docId)

clearOrphanTargets :: SQL ()
clearOrphanTargets = do
    conn <- getConnection
    liftIO $ execute_ conn "delete from `targets` where `document` is null"

removeDocumentData :: Int64 -> SQL ()
removeDocumentData docId = do
    orphanTargets docId
    conn <- getConnection
    liftIO $ do
        execute conn "delete from `content` where `document` is ?" (Only docId)
        execute conn "delete from `codes` where `document` is ?" (Only docId)

insertDocument :: FilePath -> Document -> SQL ()
insertDocument rel_path Document{..} = do
    conn <- getConnection
    docId' <- getDocumentId rel_path
    docId <- case docId' of
        Just docId -> do
            logDebug $ display $ "Replacing '" <> T.pack rel_path <> "'."
            removeDocumentData docId >> return docId
        Nothing    -> do
            logDebug $ display $ "Inserting new '" <> T.pack rel_path <> "'."
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
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM whenNothing x = maybe whenNothing return =<< x

updateCode :: ReferencePair -> SQL ()
updateCode (ref, CodeBlock {codeSource}) = do
    codeId <- fromMaybeM (throwM $ DatabaseError $ "could not find code '" <> tshow ref <> "'")
                         (queryCodeId ref)
    conn   <- getConnection
    liftIO $ execute conn "update `codes` set `source` = ? where `id` is ?" (codeSource, codeId)

updateTarget :: [ReferencePair] -> SQL ()
updateTarget refs = withTransactionM $ mapM_ updateCode refs
```

## Queries

``` {.haskell #database-queries}
listOrphanTargets :: SQL [FilePath]
listOrphanTargets = do
    conn <- getConnection
    map fromOnly <$>
        liftIO (query_ conn "select `filename` from `targets` where `document` is null" :: IO [Only FilePath])

listTargetFiles :: SQL [FilePath]
listTargetFiles = do
    conn <- getConnection
    map fromOnly <$>
        liftIO (query_ conn "select `filename` from `targets` where `document` is not null" :: IO [Only FilePath])
```

``` {.haskell #database-queries}
listSourceFiles :: SQL [FilePath]
listSourceFiles = do
    conn <- getConnection
    map fromOnly <$>
        liftIO (query_ conn "select `filename` from `documents`" :: IO [Only FilePath])
```

``` {.haskell #database-queries}
queryTargetRef :: FilePath -> SQL (Maybe (ReferenceName, Text))
queryTargetRef rel_path = do
    conn <- getConnection
    val  <- expectUnique =<< liftIO (query conn targetQuery (Only rel_path))
    return $ case val of
        Nothing           -> Nothing
        Just (name, lang) -> Just (ReferenceName name, lang)
    where targetQuery = "select `codename`,`language` from `targets` where `filename` is ?"
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
                                 \    on `code` is `codes`.`id` \
                                 \  where `content`.`document` is ?" (Only docId) :: IO [Only Text])
    return $ T.unlines $ map fromOnly result
```

### References

``` {.haskell #database-queries}
queryReferenceMap :: Config -> SQL ReferenceMap
queryReferenceMap cfg = do
        conn <- getConnection
        rows <- liftIO (query_ conn "select `documents`.`filename`, `name`, `ordinal`, `source`, `linenumber`, `language`\
                                    \from `codes` inner join `documents` on `codes`.`document` is `documents`.`id`"
                        :: IO [(FilePath, Text, Int, Text, Maybe Int, Text)])
        M.fromList <$> mapM refpair rows
    where refpair (rel_path, name, ordinal, source, linenum, lang) = do
            let lang' = maybe (UnknownClass lang) (const $ KnownLanguage lang)
                              (languageFromName cfg lang)
            return ( ReferenceId rel_path (ReferenceName name) ordinal
                   , CodeBlock lang' [] source linenum )
```

### Document
