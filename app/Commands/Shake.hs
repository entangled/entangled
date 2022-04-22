module Commands.Shake where

import RIO
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
    { once :: Bool }

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

runEnv :: MonadUnliftIO m => RIO Env a -> m a
runEnv x = do
    logOptions <- logOptionsHandle stderr True
    withLogFunc logOptions (\logFunc -> do
        withWatchManager (\wm -> do
            ch <- newChan
            let env = Env wm ch logFunc
            runRIO env x))

runAction :: Config -> [FilePath] -> RIO Env ()
runAction cfg tgts = do
    actions <- either throwM return $ immediateActions cfg
    liftIO $ shake shakeOptions (mapM_ enter actions >> want tgts)

mainLoop :: FilePath -> RIO Env ()
mainLoop path = do
    cfg <- loadIncludes =<< readConfig path
    chan <- view eventChannel
    stop <- monitor $ map (\MS.Data.Watch{..} -> (paths, \_ -> return target)) (MS.Data.watches cfg)
    target <- readChan chan
    stop
    case target of
        (MS.Data.File path) -> do
            logDebug $ "building " <> display path
            runAction cfg [T.unpack path]
        _           -> return ()
    mainLoop path

runMain :: FilePath -> RIO Env ()
runMain path = do
    cfg <- loadIncludes =<< readConfig path
    runAction cfg []
run :: (HasLogFunc env) => Args a -> RIO env ()
run = do
    
