# Main program

The main program runs the daemon, but also provides a number of commands to inspect and manipulate the database.

``` {.haskell #main-imports}
<<import-text>>
import qualified Data.Text.IO as T.IO
import qualified Data.Map.Lazy as LM

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Catch
import System.Directory
import System.FilePath
```

## Encoding

On linux consoles we use unicode bullet points (`•`). On Windows, those will just be asterisks (`*`). To facilitate this, we have to enable UTF-8 encoding.

``` {.haskell #main-imports}
import GHC.IO.Encoding
```

``` {.haskell #main-set-encoding}
setLocaleEncoding utf8
```

## Options

Options are parsed using `optparse-applicative`.

``` {.haskell #main-imports}
import Options.Applicative
```

All true options are left to the sub-commands. We're leaving `<<sub-commands>>` to be expanded.

``` {.haskell #main-options}
data Args = Args
    { versionFlag :: Bool
    , subCommand :: SubCommand }

data SubCommand
    = NoCommand
    <<sub-commands>>
```

The same goes for the sub-command parsers, which are collected in `<<sub-parsers>>`.

``` {.haskell #main-options}
parseNoCommand :: Parser SubCommand
parseNoCommand = pure NoCommand

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> ( subparser ( mempty
          <<sub-parsers>>
        ) <|> parseNoCommand )
```

And the runners.

``` {.haskell #main-imports}
import Config
```

``` {.haskell #main-run}
run :: Args -> IO ()
run Args{..}
    | versionFlag       = putStrLn "enTangleD 1.0.0"
    | otherwise         = do
        config <- configStack
        case subCommand of
            NoCommand -> return ()
            <<sub-runners>>
```

This way we can add sub-commands independently in the following sections.

### Starting the daemon

``` {.haskell #main-imports}
import Daemon
```

``` {.haskell #sub-commands}
| CommandDaemon DaemonArgs
```

``` {.haskell #sub-parsers}
<>  command "daemon" (info parseDaemonArgs ( progDesc "Run the entangled daemon." )) 
```

``` {.haskell #main-options}
data DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon <$> DaemonArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
```

``` {.haskell #sub-runners}
CommandDaemon a -> runSession config
```

### Printing the config

``` {.haskell #main-imports}
import qualified Toml
```

``` {.haskell #sub-commands}
| CommandConfig
```

``` {.haskell #sub-parsers}
<> command "config" (info (pure CommandConfig <**> helper) 
                          (progDesc "Print the default configuration."))
```

``` {.haskell #sub-runners}
CommandConfig -> T.IO.putStrLn $ Toml.encode configCodec config
```

### Inserting files to the database

``` {.haskell #sub-commands}
| CommandInsert InsertArgs
```

``` {.haskell #sub-parsers}
<> command "insert" (info parseInsertArgs ( progDesc "Insert markdown files into database." ))
```

``` {.haskell #main-options}
data InsertArgs = InsertArgs
    { insertFiles :: [FilePath] }

parseInsertArgs :: Parser SubCommand
parseInsertArgs = CommandInsert <$> InsertArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
```

``` {.haskell #sub-runners}
CommandInsert a -> printLogger $ runInsert config (insertFiles a)
```

### Tangling a single reference

``` {.haskell #sub-commands}
| CommandTangle TangleArgs
```

``` {.haskell #sub-parsers}
<> command "tangle" (info (CommandTangle <$> parseTangleArgs) ( progDesc "Retrieve tangled code." ))
```

``` {.haskell #main-options}
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
```

``` {.haskell #sub-runners}
CommandTangle a -> printLogger $ runTangle config a
```

### Stitching a markdown source

``` {.haskell #sub-commands}
| CommandStitch StitchArgs
```

``` {.haskell #sub-parsers}
<> command "stitch" (info (CommandStitch <$> parseStitchArgs) ( progDesc "Retrieve stitched markdown." ))
```

``` {.haskell #main-options}
data StitchArgs = StitchArgs
    { stitchTarget :: FilePath
    } deriving (Show)

parseStitchArgs :: Parser StitchArgs
parseStitchArgs = StitchArgs
    <$> argument str ( metavar "TARGET" )
    <**> helper
```

``` {.haskell #sub-runners}
CommandStitch a -> printLogger $ runStitch config a
```

### Listing all target files

``` {.haskell #sub-commands}
| CommandList
```

``` {.haskell #sub-parsers}
<> command "list" (info (pure CommandList <**> helper) ( progDesc "List generated code files." ))
```

``` {.haskell #sub-runners}
CommandList -> printLogger $ runList config
```

## Main

``` {.haskell file=app/Main.hs}
module Main where

<<main-imports>>

import System.Exit
import Document
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

<<main-run>>
```

## Generics

### Create the empty database

``` {.haskell #main-imports}
import Database
import Database.SQLite.Simple
```

``` {.haskell #main-run}
schema :: IO [Query]
schema = do
    qs <-  T.splitOn ";" <$> T.IO.readFile "schema.sql"
    return $ map Query (init qs)

createTables :: SQL ()
createTables = do
    conn <- getConnection
    liftIO $ schema >>= mapM_ (execute_ conn)
```

### Simple IO logger

``` {.haskell #main-imports}
import Logging
```

``` {.haskell #main-run}
newtype LoggerIO a = LoggerIO { printLogger :: (IO a) }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadThrow )

instance MonadLogger LoggerIO where
    logEntry level x = liftIO $ T.IO.putStrLn $ tshow level <> ": " <> x
```

## Tangle

``` {.haskell #main-run}
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
```

## Stitch

``` {.haskell #main-run}
runStitch :: Config -> StitchArgs -> LoggerIO ()
runStitch config StitchArgs{..} = do 
    dbPath <- getDatabasePath config
    text <- withSQL dbPath $ do 
        createTables
        stitchDocument stitchTarget
    liftIO $ T.IO.putStrLn text
```

## List

``` {.haskell #main-run}
runList :: Config -> LoggerIO ()
runList cfg = do
    dbPath <- getDatabasePath cfg
    lst <- withSQL dbPath $ do 
        createTables
        listTargetFiles
    liftIO $ T.IO.putStrLn $ T.unlines $ map T.pack lst
```

## Insert

``` {.haskell #main-run}
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

