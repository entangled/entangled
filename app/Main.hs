-- ------ language="Haskell" file="app/Main.hs"
module Main where

-- ------ begin <<main-imports>>[0]
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import qualified Data.Text.IO as T.IO
import Control.Monad.IO.Class
import Control.Monad.Reader
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
import Database.SQLite.Simple
import Database
import Config
import Tangle (parseMarkdown)
import TextUtil

-- ------ begin <<main-options>>[0]
data Args = Args
    { versionFlag :: Bool
    , subCommand :: SubCommand }

data SubCommand
    = CommandInsert InsertArgs
    | CommandDaemon DaemonArgs
    | NoCommand

parseNoCommand :: Parser SubCommand
parseNoCommand = pure NoCommand

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> ( subparser
          (  command "daemon" (info parseDaemonArgs ( fullDesc )) 
          <> command "insert" (info parseInsertArgs ( fullDesc )) )
        <|> parseNoCommand )
-- ------ end
-- ------ begin <<main-options>>[1]
data DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon <$> DaemonArgs
    <$> many (argument str (metavar "FILES..."))
-- ------ end
-- ------ begin <<main-options>>[2]
data InsertArgs = InsertArgs
    { insertFiles :: [FilePath] }

parseInsertArgs :: Parser SubCommand
parseInsertArgs = CommandInsert <$> InsertArgs
    <$> many (argument str (metavar "FILES..."))
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
    | otherwise         = case subCommand of
        CommandDaemon a -> do
            config <- configStack
            runSession config (inputFiles a)
        CommandInsert a -> do
            config <- configStack
            runInsert config (insertFiles a)
        NoCommand -> do
            return ()
    where runSession config files = putStrLn $ show config

-- ------ begin <<run-insert>>[0]
withSQL :: FilePath -> SQL a -> IO a
withSQL p (SQL x) = withConnection p (liftIO . runReaderT x)

schema :: IO [Query]
schema = do
    qs <-  T.splitOn ";" <$> T.IO.readFile "schema.sql"
    return $ map Query (init qs)

createTables :: SQL ()
createTables = do
    conn <- getConnection
    liftIO $ schema >>= mapM_ (execute_ conn)

runInsert :: Config -> [FilePath] -> IO ()
runInsert cfg files = do
    dbPath <- case configEntangled cfg >>= database of
        Nothing -> putStrLn "database not configured" >> exitFailure
        Just db -> return $ T.unpack db
    createDirectoryIfMissing True (takeDirectory dbPath)
    putStrLn $ "inserting files: " <> show files
    withSQL dbPath $ do
        conn <- getConnection
        liftIO $ execute_ conn "pragma synchronous = off"
        liftIO $ execute_ conn "pragma journal_mode = memory"
        createTables >> mapM_ readDoc files
    where readDoc f = do
            doc <- runReaderT (liftIO (T.IO.readFile f) >>= parseMarkdown f) cfg
            case doc of
                Left e -> liftIO $ T.IO.putStrLn ("warning: " <> tshow e)
                Right d -> insertDocument f d
-- ------ end
-- ------ end
