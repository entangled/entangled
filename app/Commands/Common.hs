module Commands.Common where

import RIO
import Options.Applicative (Parser, switch, long, short, help)
import Config (Config(..), HasConfig(..))

import Database (HasConnection, connection, createTables, db)
import Database.SQLite.Simple

data Args a = Args
    { versionFlag :: Bool
    , verboseFlag :: Bool
    , machineFlag :: Bool
    , checkFlag   :: Bool
    , preinsertFlag :: Bool
    , subArgs :: a }


parseArgs :: Parser a -> Parser (Args a)   {- HLINT ignore parseArgs -}
parseArgs subArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> switch (long "verbose" <> short 'V' <> help "Be very verbose.")
    <*> switch (long "machine" <> short 'm' <> help "Machine readable output.")
    <*> switch (long "check"   <> short 'c' <> help "Don't do anything, returns 1 if changes would be made to file system.")
    <*> switch (long "preinsert" <> short 'p' <> help "Tangle everything as a first action, default when db is in-memory.")
    <*> subArgs

{-|
  Generate a list of command-line arguments that you could wish to forward
  to further incantations of Entangled. This does not include flags that would
  produce meaningless results, like `version` and `check`.
 -}
forwardFlags :: Args a -> [Text]
forwardFlags args = catMaybes
    [ if verboseFlag args then Just "-V" else Nothing
    , if machineFlag args then Just "-m" else Nothing
    , if preinsertFlag args then Just "-p" else Nothing
    ]

withLogFunc' :: MonadUnliftIO m => Args b -> (LogFunc -> m a) -> m a
withLogFunc' args action = do
    logOptions <- setLogVerboseFormat True . setLogUseColor True
               <$> logOptionsHandle stderr (verboseFlag args)
    withLogFunc logOptions action

data Env = Env
    { connection' :: Connection
    , config'     :: Config
    , logFunc'    :: LogFunc }

instance HasConnection Env where
    connection = lens connection' (\ x y -> x { connection' = y })

instance HasConfig Env where
    config = lens config' (\x y -> x { config' = y })

instance HasLogFunc Env where
    logFuncL = lens logFunc' (\x y -> x { logFunc' = y })

