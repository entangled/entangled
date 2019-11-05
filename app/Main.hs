-- ------ language="Haskell" file="app/Main.hs"
module Main where

-- ------ begin <<main-imports>>[0]
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import qualified Data.Text.IO as T.IO
import qualified Data.Map.Lazy as LM

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import System.Directory
import System.FilePath
-- ------ end
-- ------ begin <<main-imports>>[1]
import GHC.IO.Encoding
-- ------ end
-- ------ begin <<main-imports>>[2]
import Options.Applicative
-- ------ end

import System.Exit
import Document
import Database.SQLite.Simple
import Database
import Logging
import Config
import Tangle (parseMarkdown, expandedCode, annotateNaked)
import TextUtil
import Comment

-- ------ begin <<main-options>>[0]
data Args = Args
    { versionFlag :: Bool
    , subCommand :: SubCommand }

data SubCommand
    = NoCommand
    -- ------ begin <<subcommand-options>>[0]
    | CommandDaemon DaemonArgs
    -- ------ end
    -- ------ begin <<subcommand-options>>[1]
    | CommandInsert InsertArgs
    -- ------ end
    -- ------ begin <<subcommand-options>>[2]
    | CommandTangle TangleArgs
    -- ------ end
    -- ------ begin <<subcommand-options>>[3]
    | CommandStitch StitchArgs
    -- ------ end
    -- ------ begin <<subcommand-options>>[4]
    | CommandList
    -- ------ end

parseNoCommand :: Parser SubCommand
parseNoCommand = pure NoCommand

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> ( subparser ( mempty
          -- ------ begin <<subparsers>>[0]
          <>  command "daemon" (info parseDaemonArgs ( fullDesc )) 
          -- ------ end
          -- ------ begin <<subparsers>>[1]
          <> command "insert" (info parseInsertArgs ( fullDesc ))
          -- ------ end
          -- ------ begin <<subparsers>>[2]
          <> command "tangle" (info (CommandTangle <$> parseTangleArgs) ( fullDesc ))
          -- ------ end
          -- ------ begin <<subparsers>>[3]
          <> command "stitch" (info (CommandStitch <$> parseStitchArgs) ( fullDesc ))
          -- ------ end
          -- ------ begin <<subparsers>>[4]
          <> command "list" (info (pure CommandList <**> helper) ( fullDesc ))
          -- ------ end
        ) <|> parseNoCommand )
-- ------ end
-- ------ begin <<main-options>>[1]
data DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon <$> DaemonArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
-- ------ end
-- ------ begin <<main-options>>[2]
data InsertArgs = InsertArgs
    { insertFiles :: [FilePath] }

parseInsertArgs :: Parser SubCommand
parseInsertArgs = CommandInsert <$> InsertArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
-- ------ end
-- ------ begin <<main-options>>[3]
data TangleQuery = TangleFile FilePath | TangleRef Text deriving (Show)

data TangleArgs = TangleArgs
    { tangleQuery :: TangleQuery
    , tangleDecorate :: Bool
    } deriving (Show)

parseTangleArgs :: Parser TangleArgs
parseTangleArgs = TangleArgs
    <$> (   TangleFile <$> strOption ( long "file" <> short 'f'
                                      <> metavar "TARGET" <> help "file target" )
        <|> TangleRef  <$> strOption ( long "ref"  <> short 'r'
                                      <> metavar "TARGET" <> help "reference target" ) )
    <*> switch (long "decorate" <> short 'd' <> help "Decorate with stitching comments.")
    <**> helper
-- ------ end
-- ------ begin <<main-options>>[4]
data StitchArgs = StitchArgs
    { stitchTarget :: FilePath
    } deriving (Show)

parseStitchArgs :: Parser StitchArgs
parseStitchArgs = StitchArgs
    <$> argument str ( metavar "TARGET" )
    <**> helper
-- ------ end

main :: IO ()
main = do
    -- ------ begin <<main-set-encoding>>[0]
    setLocaleEncoding utf8
    -- ------ end
    run =<< execParser args
    where args = info (parseArgs <**> helper)
            (  fullDesc
            <> progDesc "Automatically tangles and untangles 'FILES...'."
            <> header   "enTangleD -- daemonised literate programming"
            )

run :: Args -> IO ()
run Args{..}
    | versionFlag       = putStrLn "enTangleD 1.0.0"
    | otherwise         = do
        config <- configStack
        case subCommand of
            CommandTangle a -> printLogger $ runTangle config a
            CommandDaemon a -> runSession config (inputFiles a)
            CommandInsert a -> printLogger $ runInsert config (insertFiles a)
            CommandStitch a -> printLogger $ runStitch config a
            CommandList -> printLogger $ runList config
            NoCommand -> return ()
    where runSession config files = putStrLn $ show config

-- ------ begin <<run-insert>>[0]
schema :: IO [Query]
schema = do
    qs <-  T.splitOn ";" <$> T.IO.readFile "schema.sql"
    return $ map Query (init qs)

createTables :: SQL ()
createTables = do
    conn <- getConnection
    liftIO $ schema >>= mapM_ (execute_ conn)

newtype LoggerIO a = LoggerIO { printLogger :: (IO a) }
    deriving ( Applicative, Functor, Monad, MonadIO )

instance MonadLogger LoggerIO where
    logEntry level x = liftIO $ T.IO.putStrLn $ tshow level <> ": " <> x

getDatabasePath :: Config -> LoggerIO FilePath
getDatabasePath cfg = do
    dbPath <- case configEntangled cfg >>= database of
        Nothing -> do
            logError "database not configured"
            liftIO exitFailure
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    return dbPath

runTangle :: Config -> TangleArgs -> LoggerIO ()
runTangle cfg TangleArgs{..} = do
    dbPath <- getDatabasePath cfg
    withSQL dbPath $ do 
        createTables
        refs <- queryReferenceMap cfg
        let annotate = if tangleDecorate then annotateComment else annotateNaked
            codes = expandedCode annotate refs
            tangleRef tgt = case codes LM.!? tgt of
                Nothing -> logError $ "Reference `" <> tshow tgt <> "` not found."
                Just (Left e) -> logError $ tshow e
                Just (Right t) -> liftIO $ T.IO.putStrLn $ t
        case tangleQuery of
            TangleRef tgt -> tangleRef (ReferenceName tgt)
            TangleFile f  -> do
                ref' <- queryTargetRef f
                case ref' of
                    Nothing  -> logError $ "Target `" <> T.pack f <> "` not found."
                    Just ref -> tangleRef ref

runStitch :: Config -> StitchArgs -> LoggerIO ()
runStitch config StitchArgs{..} = do 
    dbPath <- getDatabasePath config
    text <- withSQL dbPath $ do 
        createTables
        stitchDocument stitchTarget
    liftIO $ T.IO.putStrLn text

runList :: Config -> LoggerIO ()
runList cfg = do
    dbPath <- getDatabasePath cfg
    lst <- withSQL dbPath $ do 
        createTables
        listTargetFiles
    liftIO $ T.IO.putStrLn $ T.unlines $ map T.pack lst

runInsert :: Config -> [FilePath] -> LoggerIO ()
runInsert cfg files = do
    dbPath <- getDatabasePath cfg
    logMessage $ "inserting files: " <> tshow files
    withSQL dbPath $ createTables >> mapM_ readDoc files
    where readDoc f = do
            doc <- runReaderT (liftIO (T.IO.readFile f) >>= parseMarkdown f) cfg
            case doc of
                Left e -> liftIO $ T.IO.putStrLn ("warning: " <> tshow e)
                Right d -> insertDocument f d
-- ------ end
-- ------ end
