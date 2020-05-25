let entangled = ./config-schema.dhall
in { entangled = entangled.Config :: { database  = Some "entangled.db"
                                     , annotate  = entangled.Annotate.Project } }

