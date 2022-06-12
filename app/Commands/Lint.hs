module Commands.Lint where

import RIO
import qualified RIO.Text as Text
import Options.Applicative (Parser, helper, (<**>), flag', long, short, help, metavar, argument, str)

import Entangled (Entangled)
import Linters (allLinters, lint)
import Config (HasConfig)
import Database (HasConnection)
import FileIO (dump)

data Args = Linters [Text] | ListAvailable deriving (Show, Eq)

parseArgs :: Parser Args
parseArgs = flag' ListAvailable (long "list" <> short 'l' <> help "list available linters")
    <|> (Linters <$> many (argument str (metavar "LINTERS")))
    <**> helper

run :: (HasLogFunc env, HasConnection env, HasConfig env) => Args -> Entangled env ()
run ListAvailable = dump $ Text.unlines allLinters
run (Linters ls) = void $ lint ls

