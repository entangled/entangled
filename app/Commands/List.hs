module Commands.List where

import RIO
import qualified RIO.Text as T
import Options.Applicative (Parser, helper, (<**>))

import qualified Commands.Common as Common
import Config (HasConfig)
import Database (HasConnection, db, listTargetFiles)
import Entangled (Entangled)
import FileIO (dump)

type Args = ()

parseArgs :: Parser Args
parseArgs = pure () <**> helper

listTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
            => Entangled env ()
listTargets = dump . T.unlines . map T.pack =<< db listTargetFiles

run :: (HasConnection env, HasLogFunc env, HasConfig env)
    => Common.Args Args -> RIO env ()
run args = Common.withEntangled args listTargets

