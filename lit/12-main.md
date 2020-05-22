# Main program

The main program runs the daemon, but also provides a number of commands to inspect and manipulate the database.

``` {.haskell #main-imports}
import Prelude hiding (readFile, writeFile)
import RIO (logError, logInfo, display, LogFunc, HasLogFunc, logFuncL, lens, logOptionsHandle, runRIO, withLogFunc, stderr, view)
<<import-text>>
import qualified Data.Text.IO as T.IO
import qualified Data.Map.Lazy as LM
import Data.List (sortOn)

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Catch
import Control.Monad.Logger (logInfoN)
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

run :: Args -> IO ()
run Args{..}
    | versionFlag       = putStrLn "Entangled 1.0.0"
    | otherwise         = do
        cfg <- readLocalConfig
        dbPath <- getDatabasePath cfg
        logOptions <- logOptionsHandle stderr True
        withLogFunc logOptions (\logFunc
            -> withConnection dbPath (\conn
                -> runRIO (Env conn cfg logFunc) (runEntangled $ runSubCommand subCommand)))

runSubCommand :: (HasConfig env, HasLogFunc env, HasConnection env)
              => SubCommand -> Entangled env ()
runSubCommand sc = do
    db createTables
    case sc of
        NoCommand -> return ()
        <<sub-runners>>
```

This way we can add sub-commands independently in the following sections.

### Starting the daemon

``` {.haskell #main-imports}
import Daemon (runSession)
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
CommandDaemon a -> do
    cfg <- view config
    liftIO $ runSession cfg
```

### Printing the config

``` {.haskell #main-imports}
import qualified Dhall
```

``` {.haskell #sub-commands}
| CommandConfig
```

``` {.haskell #sub-parsers}
<> command "config" (info (pure CommandConfig <**> helper) 
                          (progDesc "Print an example configuration."))
```

``` {.haskell #sub-runners}
CommandConfig -> printExampleConfig
```

### Inserting files to the database

``` {.haskell #sub-commands}
| CommandInsert InsertArgs
```

``` {.haskell #sub-parsers}
<> command "insert" (info parseInsertArgs ( progDesc "Insert markdown files into database." ))
```

``` {.haskell #main-options}
data FileType = SourceFile | TargetFile

data InsertArgs = InsertArgs
    { insertType :: FileType
    , insertFiles :: [FilePath] }

parseFileType :: Parser FileType
parseFileType = (flag' SourceFile $ long "source" <> short 's' <> help "insert markdown source file")
            <|> (flag' TargetFile $ long "target" <> short 't' <> help "insert target code file")

parseInsertArgs :: Parser SubCommand
parseInsertArgs = CommandInsert <$> (InsertArgs
    <$> parseFileType
    <*> many (argument str (metavar "FILES..."))
    <**> helper)
```

``` {.haskell #sub-runners}
CommandInsert (InsertArgs SourceFile fs) -> insertSources fs
CommandInsert (InsertArgs TargetFile fs) -> insertTargets fs
```

### Tangling a single reference

``` {.haskell #sub-commands}
| CommandTangle TangleArgs
```

``` {.haskell #sub-parsers}
<> command "tangle" (info (CommandTangle <$> parseTangleArgs) ( progDesc "Retrieve tangled code." ))
```

``` {.haskell #main-options}
data TangleArgs = TangleArgs
    { tangleQuery :: TangleQuery
    , tangleDecorate :: Bool
    } deriving (Show)

parseTangleArgs :: Parser TangleArgs
parseTangleArgs = TangleArgs
    <$> (   (TangleFile <$> strOption ( long "file" <> short 'f'
                                      <> metavar "TARGET" <> help "file target" ))
        <|> (TangleRef  <$> strOption ( long "ref"  <> short 'r'
                                      <> metavar "TARGET" <> help "reference target" ))
        <|> (flag' TangleAll $ long "all" <> short 'a' <> help "tangle all and write to disk" ))
    <*> switch (long "decorate" <> short 'd' <> help "Decorate with stitching comments.")
    <**> helper
```

``` {.haskell #sub-runners}
CommandTangle (TangleArgs {..}) -> do
    cfg <- view config
    let annotate = if tangleDecorate then (annotateComment' cfg) else annotateNaked
    tangle tangleQuery annotate
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
CommandStitch (StitchArgs {..}) -> stitch stitchTarget
```

### Listing all target files

``` {.haskell #sub-commands}
| CommandList
```

``` {.haskell #sub-parsers}
<> command "list" (info (pure CommandList <**> helper) ( progDesc "List generated code files." ))
```

``` {.haskell #sub-runners}
CommandList -> listTargets
```

### Cleaning orphan targets
This action deletes orphan targets from both the database and the file system.

``` {.haskell #sub-commands}
| CommandClearOrphans
```

``` {.haskell #sub-parsers}
<> command "clear-orphans" (info (pure CommandClearOrphans <**> helper) ( progDesc "Deletes orphan targets." ))
```

``` {.haskell #sub-runners}
CommandClearOrphans -> clearOrphans
```

## Main

``` {.haskell file=app/Main.hs}
{-# LANGUAGE LambdaCase #-}
module Main where

<<main-imports>>

-- import Paths_entangled
-- import Comment
-- import Document
-- import Select (select)
import System.Exit
import Tangle (annotateNaked, annotateComment')
-- import TextUtil
-- import FileIO
import Entangled
import Config (HasConfig)

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
import Database (HasConnection, connection, createTables, db)
import Database.SQLite.Simple
```

```
dbPath <- getDatabasePath cfg
withSQL dbPath $ do 
```

## Wiring

``` {.haskell file=src/Entangled.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Entangled where

import RIO
import RIO.Writer (MonadWriter, WriterT, runWriterT, tell)
import qualified RIO.Text as T

import qualified Data.Map.Lazy as LM

import FileIO
import Transaction

import Console (msgWrite, msgCreate, msgDelete)
import Paths_entangled
import Config (config, HasConfig, languageFromName)
import Database ( db, HasConnection, queryTargetRef, queryReferenceMap
                , listTargetFiles, insertDocument, insertTargets, stitchDocument
                , deduplicateRefs, updateTarget, listOrphanTargets, clearOrphanTargets )
import Errors (EntangledError (..))

import Comment (headerComment)
import Document (ReferenceName(..))
import Tangle (ExpandedCode, Annotator, expandedCode, parseMarkdown')
import Stitch (untangle)

type FileTransaction env = Transaction (FileIO env)

newtype Entangled env a = Entangled { unEntangled :: WriterT (FileTransaction env) (RIO env) a }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadThrow
             , MonadReader env, MonadWriter (FileTransaction env) )

runEntangled :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Entangled env a -> m a
runEntangled (Entangled x) = do
    e <- ask
    (r, w) <- runRIO e (runWriterT x)
    runFileIO' $ runTransaction w
    return r

instance (HasLogFunc env) => MonadFileIO (Entangled env) where
    readFile path       = readFile' path
    dump text           = dump' text

    writeFile path text = do
        old_content' <- liftRIO $ try $ runFileIO' $ readFile path
        case (old_content' :: Either IOException Text) of
            Right old_content | old_content == text -> return ()
                              | otherwise           -> actionw
            Left  _                                 -> actionc
        where actionw   = tell $ doc (msgWrite path)
                              <> plan (writeFile path text)
              actionc   = tell $ doc (msgCreate path)
                              <> plan (writeFile path text)

    deleteFile path     = tell $ doc (msgDelete path)
                              <> plan (deleteFile path)

data TangleQuery = TangleFile FilePath | TangleRef Text | TangleAll deriving (Show)

tangleRef :: ExpandedCode -> Annotator -> ReferenceName -> Entangled env Text
tangleRef codes annotate name =
    case codes LM.!? name of
        Nothing        -> throwM $ TangleError $ "Reference `" <> tshow name <> "` not found."
        Just (Left e)  -> throwM $ TangleError $ tshow e
        Just (Right t) -> return t

tangleFile :: (HasConnection env, HasLogFunc env, HasConfig env)
           => ExpandedCode -> Annotator -> FilePath -> Entangled env Text
tangleFile codes annotate path = do
    cfg <- view config
    (db $ queryTargetRef path) >>= \case
        Nothing              -> throwM $ TangleError $ "Target `" <> T.pack path <> "` not found."
        Just (ref, langName) -> do
            content <- tangleRef codes annotate ref
            case languageFromName cfg langName of
                Nothing -> throwM $ TangleError $ "Language unknown " <> langName
                Just lang -> return $ T.unlines [headerComment lang path, content]

tangle :: (HasConnection env, HasLogFunc env, HasConfig env)
       => TangleQuery -> Annotator -> Entangled env ()
tangle query annotate = do
    cfg <- view config
    refs <- db (queryReferenceMap cfg)
    let codes = expandedCode annotate refs
    case query of
        TangleRef ref   -> dump =<< tangleRef codes annotate (ReferenceName ref)
        TangleFile path -> dump =<< tangleFile codes annotate path
        TangleAll       -> mapM_ (\f -> writeFile f =<< tangleFile codes annotate f) =<< db listTargetFiles 

stitch :: (HasConnection env, HasLogFunc env, HasConfig env)
       => FilePath -> Entangled env ()
stitch path = dump =<< db (stitchDocument path)

listTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
            => Entangled env ()
listTargets = dump =<< (T.unlines <$> map T.pack <$> db listTargetFiles)

insertSources :: (HasConnection env, HasLogFunc env, HasConfig env)
              => [FilePath] -> Entangled env ()
insertSources files = do 
    logInfo $ display $ "inserting files: " <> tshow files
    mapM_ readDoc files
    where readDoc f = do
            doc <- parseMarkdown' f =<< readFile f
            db (insertDocument f doc)

insertTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
              => [FilePath] -> Entangled env ()
insertTargets files = do
    logInfo $ display $ "inserting files: " <> tshow files
    mapM_ readTgt files
    where readTgt f = do
            refs <- untangle f =<< readFile f
            db (updateTarget =<< deduplicateRefs refs)

clearOrphans :: (HasConnection env, HasLogFunc env, HasConfig env)
             => Entangled env ()
clearOrphans = do
    files <- db $ do
        r <- listOrphanTargets
        clearOrphanTargets
        return r
    mapM_ deleteFile files

printExampleConfig :: (HasLogFunc env)
                   => Entangled env ()
printExampleConfig = dump =<< readFile =<< liftIO (getDataFileName "data/example-config.dhall")
```

