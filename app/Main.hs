module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Config
import Daemon

data Args = Args
    { versionFlag      :: Bool
    , noLineDirectives :: Bool
    , inputFiles       :: [String]
    } deriving (Show)

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "version"  <> short 'v' <> help "Show version information.")
    <*> switch (long "no-lines" <>              help "Do not use line directives to point to source file.")
    <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = run =<< execParser args
    where args = info (parseArgs <**> helper)
            (  fullDesc
            <> progDesc "Automatically tangles and untangles 'FILES...'."
            <> header   "enTangleD -- daemonised literate programming"
            )

run :: Args -> IO ()
run args
    | versionFlag args       = putStrLn "enTangleD 0.2.0"
    | null (inputFiles args) = putStrLn "Need input files"
    | noLineDirectives args  = runSession (disableLineDirectives defaultConfig) (inputFiles args)
    | otherwise              = runSession                        defaultConfig  (inputFiles args)
