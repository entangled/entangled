let entangled = ./data/config-schema.dhall
let database = Some ".entangled/db"
let watchList = [ "lit/*.md" ]

let syntax : entangled.Syntax =
    { matchCodeStart       = "^```[ ]*{[^{}]*}"
    , matchCodeEnd         = "^```"
    , extractLanguage      = "```[ ]*{\\.([^{} \t]+)[^{}]*}"
    , extractReferenceName = "```[ ]*{[^{}]*#([^{} \t]*)[^{}]*}"
    , extractFileName      = "```[ ]*{[^{}]*file=([^{} \t]*)[^{}]*}" }

in { entangled = entangled.Config :: { database = database
                                     , watchList = watchList
                                     , syntax = syntax }
   }
