module Commands.Insert where

import RIO
import Options.Applicative (Parser, helper, (<**>), flag', long, short, help, metavar, argument, str)

import Database (HasConnection, db, updateTarget, deduplicateRefs, listTargetFiles)
import Config (HasConfig)
import Entangled (Entangled)
import FileIO (readFile)
import Stitch (untangle)
import qualified Commands.Common as Common

insertTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
              => [FilePath] -> Entangled env ()
insertTargets files = do
    logDebug $ display $ "inserting files: " <> tshow files
    mapM_ readTgt files
    where readTgt f = do
            refs <- untangle f =<< readFile f
            db (updateTarget =<< deduplicateRefs refs)

data FileType = SourceFile | TargetFile deriving (Show, Eq)
data Input = FileList [FilePath] | AllFiles deriving (Show, Eq)

data Args = Args
    { insertType :: FileType
    , insertFiles :: Input } deriving (Show, Eq)

parseFileType :: Parser FileType
parseFileType = flag' SourceFile (long "source" <> short 's' <> help "insert markdown source file")
            <|> flag' TargetFile (long "target" <> short 't' <> help "insert target code file")

parseInput :: Parser Input
parseInput = flag' AllFiles (long "all" <> short 'a' <> help "insert all relevant files")
         <|> (FileList <$> many (argument str (metavar "FILES...")))

parseArgs :: Parser Args
parseArgs = Args
    <$> parseFileType
    <*> parseInput
    <**> helper

run :: Args -> Entangled Common.Env ()
run (Args SourceFile AllFiles) = Common.preinsert
run (Args SourceFile (FileList x)) = Common.insertSources x
run (Args TargetFile AllFiles) = db listTargetFiles >>= insertTargets
run (Args TargetFile (FileList x)) = insertTargets x
    
