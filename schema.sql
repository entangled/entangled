-- ------ language="SQLite" file="schema.sql"
-- ------ begin <<schema>>[0]
create table if not exists "documents"
    ( "id"        integer primary key autoincrement
    , "filename"  text not null );
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
-- ------ end
-- ------ begin <<schema>>[3]
create table if not exists "targets"
    ( "filename"  text not null unique
    , "codename"  text not null
    , "document"  integer not null
    , foreign key ("codename") references "codes"("name")
    , foreign key ("document") references "documents"("id") );
-- ------ end
-- ------ end
