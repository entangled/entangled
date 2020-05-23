# Linting

``` {.haskell file=src/Linters.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Linters where

import RIO

import Entangled
import Database (HasConnection)
import Config (HasConfig)

lint :: (HasConnection env, HasLogFunc env, HasConfig env)
     => [Text] -> Entangled env ()
lint _ = return ()
```
