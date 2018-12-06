module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Config
import Daemon

data Args = Args
    { versionFlag :: Bool
    , inputFiles  :: [String]
    } deriving (Show)

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = run =<< execParser args
    where args = info (parseArgs <**> helper)
            (  fullDesc
            <> progDesc "Automatically tangles and untangles 'FILES...'."
            <> header   "TangleD -- daemonised literate programming"
            )

run :: Args -> IO ()
run args
    | versionFlag args       = putStrLn "TangleD 0.1.0.0"
    | null (inputFiles args) = putStrLn "Need input files"
    | otherwise              = runSession defaultConfig (inputFiles args)
