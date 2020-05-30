let entangled = https://raw.githubusercontent.com/entangled/entangled/v1.0.1/data/config-schema.dhall
                sha256:9fd18824499379eee53b974ca7570b3bc064fda546348d9b31841afab3b053a7

in { entangled = entangled.Config :: { database = Some ".entangled/db.sqlite"
                                     , watchList = [] : List Text
                                     }
   }

