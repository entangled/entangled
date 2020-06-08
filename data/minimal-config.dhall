let entangled = https://raw.githubusercontent.com/entangled/entangled/v1.2.0/data/config-schema.dhall
                sha256:91b014c660eb6a7facee7417d952371c69661b46070c36c8a5689fd548b23947

in { entangled = entangled.Config :: { database = Some ".entangled/db.sqlite"
                                     , watchList = [] : List Text
                                     }
   }

