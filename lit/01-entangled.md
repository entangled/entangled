---
title: Entangled, literate programming Swiss army knife
author: Johan Hidding
---

Entangled makes writing literate programs easier by keeping code blocks in markdown up-to-date with generated source files. By monitoring the tangled source files, any change in the master document or source files is reflected in the other. In practice this means:

* Write well documented code using Markdown.
* Use any programming language you like (or are forced to use).
* Keep debugging and using other IDE features without change.
* Generate a report in PDF or HTML from the same source (see examples on the right).

# Preliminaries

## Modules

Entangled uses `RIO`. This saves a lot of standard import statements and comes with all the perks of the Reader-IO pattern and `UnliftIO`. In one instance we will be using lazy maps though.

``` {.haskell #import-lazy-map}
import qualified Data.Map.Lazy as LM
```

We will be using the `Text` module everywhere. All parsing will be done through `Megaparsec`. We use `FSNotify` to watch events on the filesystem.

## Errors

``` {.haskell file=src/Errors.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Errors where

import RIO

data EntangledError
    = TangleError Text
    | StitchError Text
    | ReferenceError Text
    | CyclicReference Text
    | UnknownLanguageClass Text
    | DatabaseError Text
    | SystemError Text
    | MissingLanguageClass
    | NotYetImplemented Text
    | ConfigError Text
    | UnknownError
    deriving (Show, Ord, Eq, Typeable)

toEntangledError :: (Show e)
                 => (Text -> EntangledError) -> Either e a
                 -> Either EntangledError a
toEntangledError _ (Right x) = Right x
toEntangledError f (Left x) = Left $ f $ tshow x

instance Exception EntangledError

formatError :: EntangledError -> Text
formatError (TangleError t) = "tangling: " <> t
formatError (StitchError t) = "stitching: " <> t
formatError x = tshow x
```

# Design

Entangled is a command-line tool, structured around a SQLite database. The daemon ties up the different command-line facilities in a loop managed by `FSNotify`.

