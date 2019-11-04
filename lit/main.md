# Main program

``` {.haskell #main-imports}
<<import-text>>
import qualified Data.Text.IO as T.IO
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import System.Directory
import System.FilePath
```

## Encoding

``` {.haskell #main-imports}
import GHC.IO.Encoding
```

``` {.haskell #main-set-encoding}
setLocaleEncoding utf8
```

## Options

``` {.haskell #main-imports}
import Options.Applicative
```

``` {.haskell #main-options}
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
```

### Starting the daemon

``` {.haskell #main-options}
data DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon <$> DaemonArgs
    <$> many (argument str (metavar "FILES..."))
```

### Inserting files to the database

``` {.haskell #main-options}
data InsertArgs = InsertArgs
    { insertFiles :: [FilePath] }

parseInsertArgs :: Parser SubCommand
parseInsertArgs = CommandInsert <$> InsertArgs
    <$> many (argument str (metavar "FILES..."))
```

## Main

``` {.haskell file=app/Main.hs}
module Main where

<<main-imports>>

import System.Exit
import Database.SQLite.Simple
import Database
import Logging
import Config
import Tangle (parseMarkdown)
import TextUtil

<<main-options>>

main :: IO ()
main = do
    <<main-set-encoding>>
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
            printLogger $ runInsert config (insertFiles a)
        NoCommand -> do
            return ()
    where runSession config files = putStrLn $ show config

<<run-insert>>
```

## Insert

Inserts files into the database.

``` {.haskell #run-insert}
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

runInsert :: Config -> [FilePath] -> LoggerIO ()
runInsert cfg files = do
    dbPath <- case configEntangled cfg >>= database of
        Nothing -> do
            logError "database not configured"
            liftIO exitFailure
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    logMessage $ "inserting files: " <> tshow files
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
```

