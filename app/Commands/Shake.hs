{-# LANGUAGE MultiParamTypeClasses #-}

module Commands.Shake where

import RIO
import qualified RIO.Text as T
import Milkshake
    ( readConfig, loadIncludes, immediateActions, shake, shakeOptions, monitor, withWatchManager, want, enter
    , HasWatchManager, HasEventChannel(..), Config )
import qualified Milkshake as MS
import qualified Milkshake.Data as MS.Data
import Options.Applicative (Parser, switch, long, short, help)
import qualified Commands.Common as Common

helpText :: Text
helpText = "Runs a Milkshake loop, following config in '(./entangled.dhall).milkshake'"

data Args = Args
    { runOnce :: Bool }

parseArgs :: Parser Args
parseArgs = Args <$> switch (long "once" <> short '1' <> help "Run Milkshake in batch mode.")

data Env = Env
    { _watchManager :: MS.WatchManager
    , _channel      :: Chan MS.Data.Target
    , _logger       :: LogFunc
    }

instance HasWatchManager Env where
    watchManager = lens _watchManager (\e m -> e { _watchManager = m })

instance HasLogFunc Env where
    logFuncL = lens _logger (\e l -> e { _logger = l })

instance HasEventChannel Env MS.Data.Target where
    eventChannel = lens _channel (\e c -> e { _channel = c })

withEnv :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => RIO Env a -> m a
withEnv action =
    withWatchManager (\wm -> do
        ch <- newChan
        logFunc <- view logFuncL
        runRIO (Env wm ch logFunc) action)
        
runAction :: Config -> [FilePath] -> RIO Env ()
runAction cfg tgts = do
    actions <- either throwM return $ immediateActions cfg
    liftIO $ shake shakeOptions (mapM_ enter actions >> want tgts)

mainLoop :: Text -> RIO Env ()
mainLoop script = do
    cfg <- loadIncludes =<< readConfig script
    chan <- view eventChannel
    stop <- monitor $ map (\MS.Data.Watch{..} -> (paths, \_ -> return target)) (MS.Data.watches cfg)
    target <- readChan chan
    stop
    case target of
        (MS.Data.File path) -> do
            logDebug $ "building " <> display path
            runAction cfg [T.unpack path]
        _           -> return ()
    mainLoop script

runMain :: Text -> RIO Env ()
runMain path = do
    cfg <- loadIncludes =<< readConfig path
    runAction cfg []

run :: (HasLogFunc env) => Common.Args Args -> RIO env ()
run args = withEnv $ if (runOnce $ Common.subArgs args)
                       then runMain "(./entangled.dhall).milkshake"
                       else mainLoop "(./entangled.dhall).milkshake"

