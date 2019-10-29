# Database
We use an SQLite database to manage document content. Using SQL requires a remapping of the available data.

``` {.haskell file=src/Database.hs}
{-# LANGUAGE DeriveGeneric, OverloadedLabels #-}
module Database where

<<database-imports>>
<<database-io>>
```

``` {.haskell #database-imports}
import Database.SQLite.Simple
<<import-text>>
```

::: {.note}
use `pragma foreign_keys = true`.
:::

``` {.sqlite #schema}
create table "documents"
    ( "id"        integer primary key
    , "filename"  text not null );

<<table-codes>>

create table "targets"
    ( "filename"  text not null
    , "codename"  text not null
    , foreign key ("codename") references "codes"("name") );

create table "content"
    ( "id"          integer primary key
    , "document"    integer not null
    , "plain"       text
    , "codeName"    text
    , "codeOrdinal" integer
    , foreign key ("document") references "documents"("id")
    , foreign key ("codeName") references "codes"("name")
    , foreign key ("codeOrdinal") references "codes"("ordinal")
    , check ("plain" is not null or ("codeName" is not null and "codeOrdinal" is not null) );
```

A diagram for this schema, generated with `sqleton`. It would be awesome if the following would work.

``` {.make #-knit- .-hidden-}
schema.svg: <<file|schema>>
    cat $< | sqlite3 database
    sqleton -o $@ database -e -L circo 
```

![Schema](schema.svg)

In `SQLite.Simple` the above schema becomes

::: {.TODO}
Implement the interface in Selda.
:::

## Codes

The code table maps to a `ReferencePair` containing both `CodeBlock` and `ReferenceId`. I use the `name`, `ordinal` pair as primary key.

``` {.sqlite #table-codes}
create table "codes"
    ( "name"      text not null
    , "ordinal"   integer not null
    , "source"    text not null
    , "language"  text not null
    , primary key ("name", "ordinal") );
```

``` {.haskell #database-io}
data codeRow = 

addCodes :: (MonadState Connection m
            , MonadIO m)
         => [ReferencePair] -> m ()
addCodes _ = mempty

addDocument :: ( MonadState Connection m
               , MonadIO m )
            => FilePath -> Document -> m ()
addDocument rel_path Document{..} = do

```
