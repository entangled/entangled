module Commands.ClearOrphans where

import RIO

import Entangled (Entangled)
import Config (HasConfig)
import Database (HasConnection, db, listOrphanTargets, clearOrphanTargets)
import FileIO (deleteFile)

clearOrphans :: (HasConnection env, HasLogFunc env, HasConfig env)
             => Entangled env ()
clearOrphans = do
    files <- db $ do
        r <- listOrphanTargets
        clearOrphanTargets
        return r
    mapM_ deleteFile files

run :: (HasConnection env, HasLogFunc env, HasConfig env)
    => Entangled env ()
run = clearOrphans

