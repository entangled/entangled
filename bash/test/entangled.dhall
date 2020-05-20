let entangled = https://raw.githubusercontent.com/entangled/entangled/develop/data/config-schema.dhall
                sha256:2bc506e1dda5b0d3db168c68d3da09879103a5c533ee91a3ef6edbd55613d99e
in { entangled = entangled.Config :: { database  = Some "entangled.db" } }

