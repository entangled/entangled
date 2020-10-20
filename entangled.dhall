let entangled = ./data/config-schema.dhall
let database = Some ".entangled/db"
let watchList = [ "lit/*.md" ]

{- Syntax
   ------

   (new in v1.2.0) You can configure on which syntax Entangled triggers.
   Entangled works entirely on single line matches. From a single line it
   needs to extract a language, reference name and/or a file name.
   The following is the default syntax which works well with Pandoc. However,
   there are other renderers that do not work well with that syntax. In that
   case you may whish to change these settings.
 -}
let syntax : entangled.Syntax =
    { matchCodeStart       = "^```[ ]*{[^{}]*}"
    , matchCodeEnd         = "^```"
    , extractLanguage      = "^[ ]*```[ ]*{\\.([^{} \t]+)[^{}]*}"
    , extractReferenceName = "^[ ]*```[ ]*{[^{}]*#([^{} \t]*)[^{}]*}"
    , extractFileName      = "^[ ]*```[ ]*{[^{}]*file=([^{} \t]*)[^{}]*}" }

{- Database
   --------
  
   Entangled is powered by SQLite3. This specifies where the database is
   stored.  An entry of `None Text`, which is the default, keeps the database
   in memory, but this way you cannot insert new files on a running daemon.
  -}
let database = Some ".entangled/db.sqlite"
in { entangled = entangled.Config :: { database = database
                                     , watchList = watchList
                                     , syntax = syntax }
   }
