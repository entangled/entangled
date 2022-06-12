module Commands.List where

import RIO
import qualified RIO.Text as T

import Config (HasConfig)
import Database (HasConnection, db, listTargetFiles)
import Entangled (Entangled)
import FileIO (dump)

listTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
            => Entangled env ()
listTargets = dump . T.unlines . map T.pack =<< db listTargetFiles

run :: (HasConnection env, HasLogFunc env, HasConfig env)
    => Entangled env ()
run = listTargets

