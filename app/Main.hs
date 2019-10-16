-- ------ language="Haskell" file="app/Main.hs"
module Main where

import Options.Applicative
-- import Data.Semigroup ((<>))
import GHC.IO.Encoding

import Config
-- import Daemon

data Args = Args
    { versionFlag :: Bool
    , inputFiles  :: [String]
    } deriving (Show)

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = do
    setLocaleEncoding utf8
    run =<< execParser args
    where args = info (parseArgs <**> helper)
            (  fullDesc
            <> progDesc "Automatically tangles and untangles 'FILES...'."
            <> header   "enTangleD -- daemonised literate programming"
            )

run :: Args -> IO ()
run args
    | versionFlag args       = putStrLn "enTangleD 1.0.0"
    | otherwise              = do
        config <- configStack
        runSession config (inputFiles args)
    where runSession config files = putStrLn $ show config
-- ------ end
