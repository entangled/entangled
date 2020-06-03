let entangled = ./data/config-schema.dhall
let database = Some ".entangled/db"
let watchList = [ "lit/*.md" ]

in { entangled = entangled.Config :: { database = database
                                     , watchList = watchList } }

