-- ~\~ language=SQLite filename=data/schema.sql
-- ~\~ begin <<lit/03-database.md|data/schema.sql>>[0]
-- rules when editing this schema:
-- * always use double quotes for identifiers,
-- * align types for easy reading
pragma synchronous = off;
pragma journal_mode = memory;
pragma foreign_keys = on;

-- ~\~ begin <<lit/03-database.md|schema>>[0]
-- this table should be sorted on order of inclusion
create table if not exists "documents"
    ( "id"        integer primary key autoincrement
    , "filename"  text not null
    , "time"      timestamp default current_timestamp not null
    );
-- ~\~ end
-- ~\~ begin <<lit/03-database.md|schema>>[1]
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
-- ~\~ end
-- ~\~ begin <<lit/03-database.md|schema>>[2]
create table if not exists "classes"
    ( "class"     text not null
    -- ~\~ begin <<lit/03-database.md|reference-code>>[0]
    , "code"        text not null
    , foreign key ("code") references "codes"("id") on delete cascade
    -- ~\~ end
    );

create table if not exists "attributes"
    ( "attribute"   text not null
    , "value"       text not null
    -- ~\~ begin <<lit/03-database.md|reference-code>>[0]
    , "code"        text not null
    , foreign key ("code") references "codes"("id") on delete cascade
    -- ~\~ end
    );
-- ~\~ end
-- ~\~ begin <<lit/03-database.md|schema>>[3]
create table if not exists "content"
    ( "id"          integer primary key autoincrement
    , "document"    integer not null
    , "plain"       text
    , "code"        integer
    , foreign key ("document") references "documents"("id")
    , foreign key ("code") references "codes"("id")
    );
    -- , check ("plain" is not null or ("codeName" is not null and "codeOrdinal" is not null)) )
-- ~\~ end
-- ~\~ begin <<lit/03-database.md|schema>>[4]
create table if not exists "targets"
    ( "filename"  text primary key
    , "codename"  text not null
    , "language"  text not null
    , "document"  integer          -- in case this is null, the target is orphaned
    , "time"      timestamp default current_timestamp not null
    -- , foreign key ("codename") references "codes"("name")
    , foreign key ("document") references "documents"("id")
    );
-- ~\~ end
-- vim:ft=mysql
-- ~\~ end
