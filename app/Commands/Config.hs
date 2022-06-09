module Commands.Config where

import RIO
import qualified Data.Text.IO as T.IO
import Options.Applicative (Parser, switch, long, short, help, helper, (<**>))

import Paths_entangled
import qualified Commands.Common as Common

newtype Args = Args
    { minimalConfig :: Bool
    } deriving (Show, Eq)

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "minimal" <> short 'm' <> help "Print minimal config.")
    <**> helper

run :: (MonadIO m) => Common.Args Args -> m ()
run args = do
    let path = if minimalConfig $ Common.subArgs args
                  then "data/minimal-config.dhall"
                  else "data/example-config.dhall"
    liftIO $ T.IO.putStr =<< T.IO.readFile =<< getDataFileName path

