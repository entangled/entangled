let entangled = https://raw.githubusercontent.com/entangled/entangled/develop/data/config-schema.dhall
                sha256:f4c0b062f6fdb0585467c0aa0892cb50802e75979b6abc027709445785981868
in { entangled = entangled.Config :: { database  = Some "entangled.db" } }

