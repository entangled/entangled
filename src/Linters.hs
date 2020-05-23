-- ~\~ language=Haskell filename=src/Linters.hs
-- ~\~ begin <<lit/a5-linting.md|src/Linters.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Linters where

import RIO

import Entangled
import Database (HasConnection)
import Config (HasConfig)

lint :: (HasConnection env, HasLogFunc env, HasConfig env)
     => [Text] -> Entangled env ()
lint _ = return ()
-- ~\~ end
