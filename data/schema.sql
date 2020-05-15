-- ------ language="SQLite" file="data/schema.sql" project://lit/03-database.md#100
-- rules when editing this schema:
-- * always use double quotes for identifiers,
-- * align types for easy reading
pragma synchronous = off;
pragma journal_mode = memory;
pragma foreign_keys = on;

-- ------ begin <<schema>>[0] project://lit/03-database.md#132
-- this table should be sorted on order of inclusion
create table if not exists "documents"
    ( "id"        integer primary key autoincrement
    , "filename"  text not null
    , "time"      timestamp default current_timestamp not null
    );
-- ------ end
-- ------ begin <<schema>>[1] project://lit/03-database.md#144
create table if not exists "codes"
    ( "id"        integer primary key autoincrement
    , "name"      text not null
    , "ordinal"   integer not null
    , "source"    text not null
    , "language"  text not null
    , "document"  integer not null
    , foreign key ("document") references "documents"("id")
    );
-- ------ end
-- ------ begin <<schema>>[2] project://lit/03-database.md#185
create table if not exists "classes"
    ( "class"     text not null
    -- ------ begin <<reference-code>>[0] project://lit/03-database.md#175
    , "code"      text not null
    , foreign key ("code") references "codes"("id") on delete cascade
    -- ------ end
    );

create table if not exists "attributes"
    ( "attribute"   text not null
    , "value"       text not null
    -- ------ begin <<reference-code>>[0] project://lit/03-database.md#175
    , "code"        text not null
    , foreign key ("code") references "codes"("id") on delete cascade
    -- ------ end
    );
-- ------ end
-- ------ begin <<schema>>[3] project://lit/03-database.md#202
create table if not exists "content"
    ( "id"          integer primary key autoincrement
    , "document"    integer not null
    , "plain"       text
    , "code"        integer
    , foreign key ("document") references "documents"("id")
    , foreign key ("code") references "codes"("id")
    );
    -- , check ("plain" is not null or ("codeName" is not null and "codeOrdinal" is not null)) )
-- ------ end
-- ------ begin <<schema>>[4] project://lit/03-database.md#229
create table if not exists "targets"
    ( "filename"  text not null unique
    , "codename"  text not null
    , "language"  text not null
    , "document"  integer not null
    , "time"      timestamp default current_timestamp not null
    -- , foreign key ("codename") references "codes"("name")
    , foreign key ("document") references "documents"("id")
    );
-- ------ end
-- vim:ft=mysql
-- ------ end
