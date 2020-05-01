let Comment : Type = < Line : Text | Block : { start : Text, end : Text } >
let Language : Type = { name : Text, identifiers : List Text, comment : Comment, jupyter : Optional Text }

let Config : Type =
    { languages : List Language
    , watchList : Optional (List Text)
    , database  : Optional Text }

let hashComment         : Comment = Comment.Line "#"
let lispStyleComment    : Comment = Comment.Line ";"
let cStyleComment       : Comment = Comment.Block { start = "/*", end = "*/" }
let cppStyleComment     : Comment = Comment.Line "//"
let haskellStyleComment : Comment = Comment.Line "--"
let mlStyleComment      : Comment = Comment.Block { start = "(*", end = "*)" }
let xmlStyleComment     : Comment = Comment.Block { start = "<!--", end = "-->" }

in { languages =
    [ { name = "CSS"
      , identifiers = ["css"]
      , comment = cStyleComment
      , jupyter = None Text }

    , { name = "Python"
      , identifiers = ["py", "python", "python3"]
      , comment = hashComment
      , jupyter = Some "python3" }

    , { name = "Scheme"
      , identifiers = ["scheme", "r6rs", "r7rs"]
      , comment = lispStyleComment
      , jupyter = Some "guile" } 

    , { name = "C++"
      , identifiers = ["cpp", "c++"]
      , comment = cppStyleComment
      , jupyter = None Text }
    ]
   , watchList = Some ["*.md"]
   , database  = None Text
   } : Config
