module Commands.Stitch where

import RIO
import Options.Applicative (Parser, helper, (<**>), strOption, flag', long, short, help, metavar)

import Database (HasConnection, db, stitchDocument, listSourceFiles)
import Config (HasConfig)
import Entangled (Entangled)
import FileIO (dump, writeFile)
import qualified Commands.Common as Common

data Query = StitchFile FilePath | StitchAll deriving (Show, Eq)

newtype Args = Args
    { target :: Query
    } deriving (Show, Eq)

parseArgs :: Parser Args
parseArgs = Args
    <$> (  ( StitchFile <$> strOption ( long "--file" <> short 'f' <> metavar "TARGET" <> help "literate source file" ) )
       <|> flag' StitchAll ( long "--all" <> short 'a' <> help "stitch all literate sources" ) )
    <**> helper

stitchFile :: (HasConnection env, HasLogFunc env, HasConfig env)
       => FilePath -> Entangled env Text
stitchFile path = db (stitchDocument path)

stitch :: (HasConnection env, HasLogFunc env, HasConfig env)
       => Query -> Entangled env ()
stitch (StitchFile path) = dump =<< stitchFile path
stitch StitchAll = mapM_ (\f -> writeFile f =<< stitchFile f) =<< db listSourceFiles

run :: Args -> Entangled Common.Env ()
run Args {..} = stitch target
