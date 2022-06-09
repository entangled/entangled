module Commands.Common where

import RIO
import RIO.List (sort)
import RIO.Directory (makeRelativeToCurrentDirectory)
import Options.Applicative (Parser, switch, long, short, help)

import Errors
    ( EntangledError(..) )
import Config
    ( Config(..), HasConfig(..), readLocalConfig, getDatabasePath
    , getInputFiles )
import Entangled
    ( Entangled, runEntangledHuman, runEntangledMachine, insertSources
    , testEntangled )
import Console
    ( Doc )
import Database
    ( HasConnection, connection, db, createTables )
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


preinsert :: (HasConfig env, HasLogFunc env, HasConnection env)
          => Entangled env ()
preinsert = do
    db createTables
    cfg <- view config
    abs_paths <- sort <$> getInputFiles cfg
    when (null abs_paths) $ throwM $ SystemError "No input files."
    rel_paths <- mapM makeRelativeToCurrentDirectory abs_paths
    insertSources rel_paths

withEnv :: Args a -> RIO Env b -> IO b
withEnv args action = do
    cfg <- readLocalConfig
    dbPath <- getDatabasePath cfg
    logOptions <- setLogVerboseFormat True . setLogUseColor True
               <$> logOptionsHandle stderr (verboseFlag args)
    withLogFunc logOptions (\logFunc
        -> withConnection dbPath (\conn
            -> runRIO (Env conn cfg logFunc) action))

withEntangled :: (HasConfig env, HasLogFunc env, HasConnection env)
              => Args s -> Entangled env a -> RIO env a
withEntangled args action = do
    cfg <- view config
    dbPath <- getDatabasePath cfg
    let preinsertFlag' = preinsertFlag args || dbPath == ":memory:"
        action' = (if preinsertFlag' then preinsert else pure ()) >> action
    if checkFlag args
    then do
        todo <- testEntangled action'
        if todo then exitFailure else exitSuccess
    else runEntangled args Nothing action'

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

runEntangled :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Args s -> Maybe Doc -> Entangled env a -> m a
runEntangled Args { machineFlag = True } _ = runEntangledMachine
runEntangled Args { machineFlag = False } h = runEntangledHuman h

