let entangled = https://raw.githubusercontent.com/entangled/entangled/v1.3.0/data/config-schema.dhall
                sha256:cb03a230547147223b6bd522c133d16d17921e249e7c2bd505d31bf7729d2cc5

in { entangled = entangled.Config ::
        { watchList = [ "**/*.md" ] : List Text
        }
   }
