let entangled = https://raw.githubusercontent.com/entangled/entangled/v1.2.0/data/config-schema.dhall
i               sha256:dbf04dd6e3f7ceedb2bdafdf5b884f8d1994cbf8a760936588a76a3925b10a7e

in { entangled = entangled.Config :: { database = Some ".entangled/db.sqlite"
                                     , watchList = [] : List Text
                                     }
   }

