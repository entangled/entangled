{- Entangled configuration file
    ============================
  
    This configuration uses the Dhall language, see https://dhall-lang.org/.
  
    This file represents a record, as can be seen from the `let ... in { ... }`
    structure. Configuration for the Entangled daemon and command-line utility
    is stored in the `entangled` member of the record. Other fields are open
    for use by `pandoc` filters or other tools.
  
    If you want to check the configuration for correctness, run
  
        dhall <<< ./entangled.dhall
  -}

{- Schema
   ------
  
   The schema is loaded here. It is recommended to use a schema from a
   released version of Entangled.
  
       let entangled = https://raw.githubusercontent.com/entangled/entangled/v1.0.0/data/config-schema.dhall
                       sha256:3fd83a15e2ed592e5e15b123cfdb955fb46cc8473f03963bbed88322c13db5bf
  
   The hash is not needed, but it ensures that you get the version you're
   expecting to, or raise an error.
  
   TODO: Update this line, since v1.0.0 is not out yet.
  -}
let entangled = https://raw.githubusercontent.com/entangled/entangled/develop/data/config-schema.dhall
                sha256:d28e56ce4d3abbbad938296f27b744c0972ad8da0b2baa68a36ec46ed074d55b               

{- Languages
   ---------
  
   Entangled has many programming languages configured by default. If your
   favourite is not in there you can add them here. The most common commenting
   patterns are included in `entangled.comments`
  
        | name         | comment pattern |
        | ------------ | --------------- |
        | hash         | `# ...`         |
        | lispStyle    | `; ...`         |
        | mlStyle      | `(* ... *)`     |
        | cStyle       | `/* ... */`     |
        | cppStyle     | `// ...`        |
        | haskellStyle | `-- ...`        |
        | texStyle     | `% ...`         |
        | xmlStyle     | `<!-- ... -->`  |
    
   The language identifiers are the classes that can be used to identify a code
   block is in a certain language. So,
  
        ``` {.c++ #hello}
        std::cout << "Hello, World!\n";
        ```
  
   is recognized as a C++ block, because `c++` is in the identifier list of the C++
   language. In this example we add some esotheric languages.
  -}
let intercalComment = entangled.Comment.Line "PLEASE NOTE: "

let languages = entangled.languages #
    [ { name = "Unlambda", identifiers = ["unlambda"], comment = entangled.comments.hash }
    , { name = "Intercal", identifiers = ["intercal"], comment = intercalComment }
    ]

{- Database
   --------
  
   Entangled is powered by SQLite3. This specifies where the database is
   stored.  An entry of `None Text`, which is the default, keeps the database
   in memory, but this way you cannot insert new files on a running daemon.
  -}
let database = Some ".entangled/db"

{- Watch list
   ----------
  
   List of glob patterns that the daemon needs to watch for changes. The order
   in which files are listed here matters. If code blocks append on previous
   blocks in different files, the blocks are concatenated in the same order as
   the files are listed here. In the case of a glob pattern, this is
   alpha-numerical order (same as `ls`).
  -}
let watchList = [ "lit/*.md" ]

in { entangled = entangled.Config :: { database = database
                                     , watchList = watchList
                                     , languages = languages } }

