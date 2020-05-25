-- ~\~ language=Dhall filename=data/config-schema.dhall
-- ~\~ begin <<lit/04-configuration.md|data/config-schema.dhall>>[0]
let Comment : Type = < Line : Text | Block : { start : Text, end : Text } >
let Language : Type = { name : Text, identifiers : List Text, comment : Comment }
let LineDirective : Type = { name : Text, format: Text }

-- ~\~ begin <<lit/04-configuration.md|config-comment-styles>>[0]
let comments =
    { hash         = Comment.Line "#"
    , lispStyle    = Comment.Line ";"
    , cStyle       = Comment.Block { start = "/*", end = "*/" }
    , cppStyle     = Comment.Line "//"
    , haskellStyle = Comment.Line "--"
    , mlStyle      = Comment.Block { start = "(*", end = "*)" }
    , xmlStyle     = Comment.Block { start = "<!--", end = "-->" }
    , texStyle     = Comment.Line "%"
    }
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-languages>>[0]
let languages =
    [ { name = "Awk",        identifiers = ["awk"],           comment = comments.hash }
    , { name = "C",          identifiers = ["c"],             comment = comments.cStyle }
    , { name = "C++",        identifiers = ["cpp", "c++"],    comment = comments.cppStyle }
    , { name = "Clojure",    identifiers = ["clojure"],       comment = comments.lispStyle }
    , { name = "CSS",        identifiers = ["css"],           comment = comments.cStyle }
    , { name = "D",          identifiers = ["d"],             comment = comments.cppStyle }
    , { name = "Dhall",      identifiers = ["dhall"],         comment = comments.haskellStyle }
    , { name = "Elm",        identifiers = ["elm"],           comment = comments.haskellStyle }
    , { name = "Gnuplot",    identifiers = ["gnuplot"],       comment = comments.hash }
    , { name = "Haskell",    identifiers = ["haskell"],       comment = comments.haskellStyle }
    , { name = "HTML",       identifiers = ["html"],          comment = comments.xmlStyle }
    , { name = "Idris",      identifiers = ["idris"],         comment = comments.haskellStyle }
    , { name = "Julia",      identifiers = ["julia"],         comment = comments.hash }
    , { name = "JavaScript", identifiers = ["js", "javascript", "ecma"],
                                                              comment = comments.cStyle }
    , { name = "LaTeX",      identifiers = ["latex"],         comment = comments.texStyle }
    , { name = "Lua",        identifiers = ["lua"],           comment = comments.haskellStyle }
    , { name = "Make",       identifiers = ["make", "makefile"],
                                                              comment = comments.hash }
    , { name = "OCaml",      identifiers = ["ocaml"],         comment = comments.mlStyle }
    , { name = "OpenCL",     identifiers = ["opencl"],        comment = comments.cStyle }
    , { name = "PureScript", identifiers = ["purs", "purescript"],
                                                              comment = comments.haskellStyle }
    , { name = "Python",     identifiers = ["py", "python"],  comment = comments.hash }
    , { name = "R",          identifiers = ["r"],             comment = comments.hash }
    , { name = "Rust",       identifiers = ["rust"],          comment = comments.cppStyle }
    , { name = "Scheme",     identifiers = ["scheme", "r6rs" ,"racket", "r7rs"],
                                                              comment = comments.lispStyle }
    , { name = "SQLite",     identifiers = ["sqlite"],        comment = comments.haskellStyle }
    , { name = "TOML",       identifiers = ["toml"],          comment = comments.hash }
    , { name = "TypeScript", identifiers = ["ts", "typescript"],
                                                              comment = comments.cppStyle }
    , { name = "YAML",       identifiers = ["yaml"],          comment = comments.hash }
    ]

let lineDirectives =
    [ { name = "C",          format = "#LINE {linenumber} \"{filename}\"" }
    , { name = "C++",        format = "#LINE {linenumber} \"{filename}\"" }
    , { name = "Haskell",    format = "{{-# LINE {linenumber} \"{filename}\" #-}}" }
    ]
-- ~\~ end

let Annotate = < Naked | Standard | Project >

let Config =
    { Type =
        { version   : Text
        , languages : List Language
        , watchList : List Text
        , database  : Optional Text
        , annotate  : Annotate
        , lineDirectives : List LineDirective
        , useLineDirectives : Bool }
    , default =
        { version   = "1.0.0"
        , languages = languages
        , watchList = [] : List Text
        , database  = None Text
        , annotate  = Annotate.Standard
        , lineDirectives = lineDirectives
        , useLineDirectives = False }
    }

in { Comment   = Comment
   , Language  = Language
   , LineDirective = LineDirective
   , Config    = Config
   , Annotate  = Annotate
   , comments  = comments
   , languages = languages
   , lineDirectives = lineDirectives
   }
-- ~\~ end
