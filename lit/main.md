# Main program

``` {.haskell #main-imports}
<<import-text>>
import qualified Data.Text.IO as T.IO
import qualified Data.Map.Lazy as LM

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
    = NoCommand
    <<subcommand-options>>

parseNoCommand :: Parser SubCommand
parseNoCommand = pure NoCommand

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> ( subparser ( mempty
          <<subparsers>>
        ) <|> parseNoCommand )
```

### Starting the daemon

``` {.haskell #subcommand-options}
| CommandDaemon DaemonArgs
```

``` {.haskell #subparsers}
<>  command "daemon" (info parseDaemonArgs ( fullDesc )) 
```

``` {.haskell #main-options}
data DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon <$> DaemonArgs
    <$> many (argument str (metavar "FILES..."))
```

### Inserting files to the database

``` {.haskell #subcommand-options}
| CommandInsert InsertArgs
```

``` {.haskell #subparsers}
<> command "insert" (info parseInsertArgs ( fullDesc ))
```

``` {.haskell #main-options}
data InsertArgs = InsertArgs
    { insertFiles :: [FilePath] }

parseInsertArgs :: Parser SubCommand
parseInsertArgs = CommandInsert <$> InsertArgs
    <$> many (argument str (metavar "FILES..."))
```

### Tangling a single reference

``` {.haskell #subcommand-options}
| CommandTangle TangleArgs
```

``` {.haskell #subparsers}
<> command "tangle" (info (CommandTangle <$> parseTangleArgs) ( fullDesc ))
```

``` {.haskell #main-options}
data TangleArgs = TangleArgs
    { tangleTarget :: Text
    , tangleDecorate :: Bool
    } deriving (Show)

parseTangleArgs :: Parser TangleArgs
parseTangleArgs = TangleArgs
    <$> argument str (metavar "TARGET")
    <*> switch (long "decorate" <> short 'd' <> help "Decorate with stitching comments.")
```

### Listing all target files

``` {.haskell #subcommand-options}
| CommandList
```

``` {.haskell #subparsers}
<> command "list"   (info (pure CommandList) ( fullDesc ))
```

## Main

``` {.haskell file=app/Main.hs}
module Main where

<<main-imports>>

import System.Exit
import Document
import Database.SQLite.Simple
import Database
import Logging
import Config
import Tangle (parseMarkdown, expandedCode, annotateNaked)
import TextUtil
import Comment

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
        CommandTangle a -> do
            config <- configStack
            printLogger $ runTangle config a
        CommandDaemon a -> do
            config <- configStack
            runSession config (inputFiles a)
        CommandInsert a -> do
            config <- configStack
            printLogger $ runInsert config (insertFiles a)
        CommandList -> do
            config <- configStack
            printLogger $ runList config
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
    refs <- withSQL dbPath $ do 
        createTables
        queryReferenceMap cfg
    let annotate = if tangleDecorate then annotateComment else annotateNaked
        codes = expandedCode annotate refs
    case codes LM.!? (ReferenceName tangleTarget) of
        Nothing -> logError $ "Reference `" <> tangleTarget <> "` not found."
        Just (Left e) -> logError $ tshow e
        Just (Right t) -> liftIO $ T.IO.putStrLn $ t

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
```

