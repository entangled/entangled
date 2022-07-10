module Commands.Init where

import RIO
import RIO.Writer (WriterT, MonadWriter, runWriterT)
-- import qualified Data.Text.IO as T.IO
import Options.Applicative (Parser, strOption, metavar, long, short, help, helper, (<**>))

import Paths_entangled
import qualified Commands.Common as Common

import Transaction ( Transaction(..), testTransaction
                   , runTransaction, runTransactionMachine )
import FileIO ( FileIO(..), runFileIO' )

type FileTransaction = Transaction (FileIO Env)
newtype Env = Env { logFunc' :: LogFunc }
newtype InitM a = InitM { getInitM :: WriterT FileTransaction (RIO Env) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadWriter FileTransaction
             , MonadReader Env )

instance HasLogFunc Env where
    logFuncL = lens logFunc' (\x y -> x { logFunc' = y })

runInitM :: (MonadUnliftIO m) => Common.Args args -> InitM () -> m ()
runInitM args@Common.Args{..} action = Common.withLogFunc' args (\logFunc -> 
    runRIO (Env logFunc) $ do
        (_, w) <- runWriterT (getInitM action)
        runFileIO' $ case (machineFlag, checkFlag) of
          (_, True)  -> do
              todo <- testTransaction w
              if todo then exitFailure else exitSuccess
          (True, _)  -> runTransactionMachine w
          (False, _) -> runTransaction (Just "initializing") w)

data Args
    = BuiltIn Text
    | Git Text
    deriving (Show, Eq)

parseArgs :: Parser Args
parseArgs = (Git <$> strOption     (  long "git" <> short 'g'
                                   <> help "Initialize from a git repo."
                                   <> metavar "REPO" ) )
        <|> (BuiltIn <$> strOption (  long "template" <> short 't'
                                   <> help "Iniitialize from a built-in template."
                                   <> metavar "TEMPLATE" ) )
        <|> pure (BuiltIn "default")
        <**> helper

run :: (MonadUnliftIO m) => Common.Args Args -> m ()
run args = runInitM args $ do
    case Common.subArgs args of
      BuiltIn n -> logInfo $ display $ "initializing from template: " <> n
      Git g     -> logInfo $ display $ "initializing from git: " <> g

