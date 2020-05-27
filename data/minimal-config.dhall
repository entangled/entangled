let entangled = https://raw.githubusercontent.com/entangled/entangled/master/data/config-schema.dhall
                sha256:a924fcffa514e3909459e48c45924886062ff4a77be582af0517f9c4bae7ae73

in { entangled = entangled.Config :: { database = Some ".entangled/db.sqlite"
                                     , watchList = []
                                     }
   }

