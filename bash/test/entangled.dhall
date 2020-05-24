let entangled = https://raw.githubusercontent.com/entangled/entangled/develop/data/config-schema.dhall
                sha256:5ed17bac2f28bceb5aa47f5c044447b869740b4f7684e01796781f40da1f3504
in { entangled = entangled.Config :: { database  = Some "entangled.db" } }

