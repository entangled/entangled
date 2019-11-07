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
-- ------ begin <<main-imports>>[3]
import Config
-- ------ end
-- ------ begin <<main-imports>>[4]
import qualified Toml
-- ------ end
-- ------ begin <<main-imports>>[5]
import Database
import Database.SQLite.Simple
-- ------ end
-- ------ begin <<main-imports>>[6]
import Logging
-- ------ end

import System.Exit
import Document
import Tangle (parseMarkdown, expandedCode, annotateNaked)
import TextUtil
import Comment

-- ------ begin <<main-options>>[0]
data Args = Args
    { versionFlag :: Bool
    , subCommand :: SubCommand }

data SubCommand
    = NoCommand
    -- ------ begin <<sub-commands>>[0]
    | CommandDaemon DaemonArgs
    -- ------ end
    -- ------ begin <<sub-commands>>[1]
    | CommandConfig
    -- ------ end
    -- ------ begin <<sub-commands>>[2]
    | CommandInsert InsertArgs
    -- ------ end
    -- ------ begin <<sub-commands>>[3]
    | CommandTangle TangleArgs
    -- ------ end
    -- ------ begin <<sub-commands>>[4]
    | CommandStitch StitchArgs
    -- ------ end
    -- ------ begin <<sub-commands>>[5]
    | CommandList
    -- ------ end
-- ------ end
-- ------ begin <<main-options>>[1]
parseNoCommand :: Parser SubCommand
parseNoCommand = pure NoCommand

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> ( subparser ( mempty
          -- ------ begin <<sub-parsers>>[0]
          <>  command "daemon" (info parseDaemonArgs ( progDesc "Run the entangled daemon." )) 
          -- ------ end
          -- ------ begin <<sub-parsers>>[1]
          <> command "config" (info (pure CommandConfig <**> helper) 
                                    (progDesc "Print the default configuration."))
          -- ------ end
          -- ------ begin <<sub-parsers>>[2]
          <> command "insert" (info parseInsertArgs ( progDesc "Insert markdown files into database." ))
          -- ------ end
          -- ------ begin <<sub-parsers>>[3]
          <> command "tangle" (info (CommandTangle <$> parseTangleArgs) ( progDesc "Retrieve tangled code." ))
          -- ------ end
          -- ------ begin <<sub-parsers>>[4]
          <> command "stitch" (info (CommandStitch <$> parseStitchArgs) ( progDesc "Retrieve stitched markdown." ))
          -- ------ end
          -- ------ begin <<sub-parsers>>[5]
          <> command "list" (info (pure CommandList <**> helper) ( progDesc "List generated code files." ))
          -- ------ end
        ) <|> parseNoCommand )
-- ------ end
-- ------ begin <<main-options>>[2]
data DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon <$> DaemonArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
-- ------ end
-- ------ begin <<main-options>>[3]
data InsertArgs = InsertArgs
    { insertFiles :: [FilePath] }

parseInsertArgs :: Parser SubCommand
parseInsertArgs = CommandInsert <$> InsertArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
-- ------ end
-- ------ begin <<main-options>>[4]
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
-- ------ begin <<main-options>>[5]
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

-- ------ begin <<main-run>>[0]
run :: Args -> IO ()
run Args{..}
    | versionFlag       = putStrLn "enTangleD 1.0.0"
    | otherwise         = do
        config <- configStack
        case subCommand of
            NoCommand -> return ()
            -- ------ begin <<sub-runners>>[0]
            CommandDaemon a -> putStrLn $ show config
            -- ------ end
            -- ------ begin <<sub-runners>>[1]
            CommandConfig -> T.IO.putStrLn $ Toml.encode configCodec config
            -- ------ end
            -- ------ begin <<sub-runners>>[2]
            CommandInsert a -> printLogger $ runInsert config (insertFiles a)
            -- ------ end
            -- ------ begin <<sub-runners>>[3]
            CommandTangle a -> printLogger $ runTangle config a
            -- ------ end
            -- ------ begin <<sub-runners>>[4]
            CommandStitch a -> printLogger $ runStitch config a
            -- ------ end
            -- ------ begin <<sub-runners>>[5]
            CommandList -> printLogger $ runList config
            -- ------ end
-- ------ end
-- ------ begin <<main-run>>[1]
getDatabasePath :: Config -> LoggerIO FilePath
getDatabasePath cfg = do
    dbPath <- case configEntangled cfg >>= database of
        Nothing -> do
            logError "database not configured"
            liftIO exitFailure
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    return dbPath
-- ------ end
-- ------ begin <<main-run>>[2]
schema :: IO [Query]
schema = do
    qs <-  T.splitOn ";" <$> T.IO.readFile "schema.sql"
    return $ map Query (init qs)

createTables :: SQL ()
createTables = do
    conn <- getConnection
    liftIO $ schema >>= mapM_ (execute_ conn)
-- ------ end
-- ------ begin <<main-run>>[3]
newtype LoggerIO a = LoggerIO { printLogger :: (IO a) }
    deriving ( Applicative, Functor, Monad, MonadIO )

instance MonadLogger LoggerIO where
    logEntry level x = liftIO $ T.IO.putStrLn $ tshow level <> ": " <> x
-- ------ end
-- ------ begin <<main-run>>[4]
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
-- ------ end
-- ------ begin <<main-run>>[5]
runStitch :: Config -> StitchArgs -> LoggerIO ()
runStitch config StitchArgs{..} = do 
    dbPath <- getDatabasePath config
    text <- withSQL dbPath $ do 
        createTables
        stitchDocument stitchTarget
    liftIO $ T.IO.putStrLn text
-- ------ end
-- ------ begin <<main-run>>[6]
runList :: Config -> LoggerIO ()
runList cfg = do
    dbPath <- getDatabasePath cfg
    lst <- withSQL dbPath $ do 
        createTables
        listTargetFiles
    liftIO $ T.IO.putStrLn $ T.unlines $ map T.pack lst
-- ------ end
-- ------ begin <<main-run>>[7]
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
