-- ------ language="SQLite" file="data/schema.sql"
-- vim:ft=sqlite
pragma synchronous = off;
pragma journal_mode = memory;
pragma foreign_keys = on;

-- ------ begin <<schema>>[0]
create table if not exists "documents"
    ( "id"        integer primary key autoincrement
    , "filename"  text not null
    , "time"      timestamp default current_timestamp not null
    );
-- ------ end
-- ------ begin <<schema>>[1]
create table if not exists "codes"
    ( "name"      text not null
    , "ordinal"   integer not null
    , "source"    text not null
    , "language"  text not null
    , "document"  integer not null
    , primary key ("name", "ordinal")
    , foreign key ("document") references "documents"("id") );
-- ------ end
-- ------ begin <<schema>>[2]
create table if not exists "classes"
    ( "class"       text not null
    -- ------ begin <<reference-code>>[0]
    , "codeName"    text not null
    , "codeOrdinal" integer not null
    , constraint "rcode" foreign key ("codeName", "codeOrdinal") references "codes"("name","ordinal") on delete cascade
    -- , foreign key ("codeName") references "codes"("name")
    -- , foreign key ("codeOrdinal") references "codes"("ordinal")
    -- ------ end
    );

create table if not exists "attributes"
    ( "attribute"   text not null
    , "value"       text not null
    -- ------ begin <<reference-code>>[0]
    , "codeName"    text not null
    , "codeOrdinal" integer not null
    , constraint "rcode" foreign key ("codeName", "codeOrdinal") references "codes"("name","ordinal") on delete cascade
    -- , foreign key ("codeName") references "codes"("name")
    -- , foreign key ("codeOrdinal") references "codes"("ordinal")
    -- ------ end
    );
-- ------ end
-- ------ begin <<schema>>[3]
create table if not exists "content"
    ( "id"          integer primary key autoincrement
    , "document"    integer not null
    , "plain"       text
    , "codeName"    text
    , "codeOrdinal" integer
    , foreign key ("document") references "documents"("id")
    , foreign key ("codeName", "codeOrdinal") references "codes"("name","ordinal")
    );
    -- , check ("plain" is not null or ("codeName" is not null and "codeOrdinal" is not null)) )
-- ------ end
-- ------ begin <<schema>>[4]
create table if not exists "targets"
    ( "filename"  text not null unique
    , "codename"  text not null
    , "document"  integer not null
    , "time"      timestamp default current_timestamp not null
    -- , foreign key ("codename") references "codes"("name")
    , foreign key ("document") references "documents"("id") );
-- ------ end
-- ------ end
