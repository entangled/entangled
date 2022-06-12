module Commands.Rules where

import RIO
-- import qualified Dhall
import qualified RIO.Text as Text

import qualified Commands.Common as Common
import Entangled (Entangled)
import Database (HasConnection, db, listTargetFiles)
import FileIO (dump)

tangleWatch :: Entangled Common.Env Text
tangleWatch = do
    paths <- Common.listSourceFiles
    return $ ", ms.watch " <> tshow paths <> " (ms.phony \"%tangle%\")"

tangleAction :: Entangled Common.Env Text
tangleAction = do
    inputPaths <- Common.listSourceFiles
    outputPaths <- db listTargetFiles
    return $ ", ms.Action [ms.phony \"%tangle%\"] " <> tshow outputPaths <> "\n"
          <> ", ms.Action " <> tshow outputPaths <> " " <> tshow inputPaths <> " "
          <> "(Some \"entangled tangle -ad\")"

stitchWatch :: Entangled Common.Env Text
stitchWatch = do
    paths <- db listTargetFiles
    return $ ", ms.watch " <> tshow paths <> " (ms.phony \"%stitch%\")"

stitchAction :: Entangled Common.Env Text
stitchAction = do
    inputPaths <- db listTargetFiles
    outputPaths <- Common.listSourceFiles
    return $ ", ms.Action [ms.phony \"%stitch%\"] " <> tshow outputPaths <> "\n"
          <> ", ms.Action " <> tshow outputPaths <> " " <> tshow inputPaths <> " "
          <> "(Some \"entangled stitch -a\")"

run :: Entangled Common.Env ()
run = dump =<< Text.unlines <$> sequence [tangleWatch, tangleAction, stitchWatch, stitchAction]

