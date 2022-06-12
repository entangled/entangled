# Main program
The main program runs the daemon, but also provides a number of commands to inspect and manipulate the database.

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
data SubCommand
    = NoCommand
    <<sub-commands>>
    deriving (Show, Eq)
```

The same goes for the sub-command parsers, which are collected in `<<sub-parsers>>`.

``` {.haskell #main-options}
parseNoCommand :: Parser SubCommand
parseNoCommand = pure NoCommand

parseSubCommand :: Parser SubCommand   {- HLINT ignore parseArgs -}
parseSubCommand = ( subparser ( mempty
          <<sub-parsers>>
        ) <|> parseNoCommand )
```

And the runners.

``` {.haskell #main-imports}
import Config
```

``` {.haskell #main-run}
run :: Common.Args SubCommand -> IO ()
run (Common.Args True _ _ _ _ _) = putStrLn $ showVersion version
run args@Common.Args{..} = 
    case subArgs of
      CommandConfig x -> Commands.Config.run (args {Common.subArgs = x})
      CommandList x   -> Common.withEnv args $ Commands.List.run (args {Common.subArgs = x})
      CommandTangle x -> Common.withEnv args $ Common.withEntangled args $ Commands.Tangle.run x
      _               -> Common.withEnv args $ Common.withEntangled args (runSubCommand subArgs)

runSubCommand :: (HasConfig env, HasLogFunc env, HasConnection env)
              => SubCommand -> Entangled env ()
runSubCommand sc = do
    db createTables
    case sc of
        NoCommand -> return ()
        <<sub-runners>>
        _    -> return ()
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
newtype DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show, Eq)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon . DaemonArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
```

``` {.haskell #sub-runners}
CommandDaemon DaemonArgs {..} -> runSession inputFiles
```

### Printing the config

``` {.haskell #main-options}

```

``` {.haskell #sub-commands}
| CommandConfig Commands.Config.Args
```

``` {.haskell #sub-parsers}
<> command "config" (info (CommandConfig <$> Commands.Config.parseArgs)
                          (progDesc "Print an example configuration."))
```

``` {.haskell #sub-runners}
CommandConfig _ -> printExampleConfig
```

### Inserting files to the database

``` {.haskell #sub-commands}
| CommandInsert InsertArgs
```

``` {.haskell #sub-parsers}
<> command "insert" (info parseInsertArgs ( progDesc "Insert markdown files into database." ))
```

``` {.haskell #main-options}
data FileType = SourceFile | TargetFile deriving (Show, Eq)

data InsertArgs = InsertArgs
    { insertType :: FileType
    , insertFiles :: [FilePath] } deriving (Show, Eq)

parseFileType :: Parser FileType
parseFileType = flag' SourceFile (long "source" <> short 's' <> help "insert markdown source file")
            <|> flag' TargetFile (long "target" <> short 't' <> help "insert target code file")

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
| CommandTangle Commands.Tangle.Args
```

``` {.haskell #sub-parsers}
<> command "tangle" (info (CommandTangle <$> Commands.Tangle.parseArgs) ( progDesc "Retrieve tangled code." ))
```

``` {.haskell #main-options}

```

``` {.haskell #sub-runners}

```

### Stitching a markdown source

``` {.haskell #sub-commands}
| CommandStitch StitchArgs
```

``` {.haskell #sub-parsers}
<> command "stitch" (info (CommandStitch <$> parseStitchArgs) ( progDesc "Retrieve stitched markdown." ))
```

``` {.haskell #main-options}
newtype StitchArgs = StitchArgs
    { stitchTarget :: FilePath
    } deriving (Show, Eq)

parseStitchArgs :: Parser StitchArgs
parseStitchArgs = StitchArgs
    <$> argument str ( metavar "TARGET" )
    <**> helper
```

``` {.haskell #sub-runners}
CommandStitch StitchArgs {..} -> stitch (StitchFile stitchTarget)
```

### Listing all target files

``` {.haskell #sub-commands}
| CommandList Commands.List.Args
```

``` {.haskell #sub-parsers}
<> command "list" (info (CommandList <$> Commands.List.parseArgs)
                        (progDesc "List generated code files." ))
```

``` {.haskell #sub-runners}
CommandList _ -> listTargets
```

### Linter

``` {.haskell #main-options}
newtype LintArgs = LintArgs
    { lintFlags :: [Text]
    } deriving (Show, Eq)

parseLintArgs :: Parser LintArgs
parseLintArgs = LintArgs
    <$> many (argument str (metavar "LINTERS"))
    <**> helper
```

``` {.haskell #sub-commands}
| CommandLint LintArgs
```

``` {.haskell #sub-parsers}
<> command "lint" (info (CommandLint <$> parseLintArgs) ( progDesc ("Lint input on potential problems. Available linters: " <> RIO.Text.unpack (RIO.Text.unwords allLinters))))
```

``` {.haskell #sub-runners}
CommandLint LintArgs {..} -> void $ liftRIO $ lint lintFlags
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
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import RIO.Text (unwords, unpack)

import Prelude (putStrLn)
import Paths_entangled
import Data.Version (showVersion)

<<main-imports>>

import Entangled
import Linters
import qualified Commands.Common as Common
import qualified Commands.Config
import qualified Commands.List
import qualified Commands.Tangle

<<main-options>>

main :: IO ()
main = do
    <<main-set-encoding>>
    run =<< execParser args
    where args = info (Common.parseArgs parseSubCommand <**> helper)
            (  fullDesc
            <> progDesc "Automatically tangles and untangles 'FILES...'."
            <> header   "Entangled -- daemonised literate programming"
            )

<<main-run>>
```

## Generics

### Create the empty database

``` {.haskell #main-imports}
import Database (HasConnection, createTables, db)
-- import Comment (annotateNaked)
```

```
dbPath <- getDatabasePath cfg
withSQL dbPath $ do 
```

## Wiring

``` {.haskell file=src/Entangled.hs}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Entangled where

import RIO
import RIO.Writer (MonadWriter, WriterT, runWriterT, tell)
import qualified RIO.Text as T

import qualified Data.Map.Lazy as LM
import Control.Monad.Except ( MonadError(..) )

import FileIO
import Transaction

import Console (Doc, timeStamp)
import Paths_entangled
import Config (config, HasConfig, languageFromName)
import Database ( db, HasConnection, queryTargetRef, queryReferenceMap
                , listTargetFiles, insertDocument, stitchDocument, listSourceFiles
                , deduplicateRefs, updateTarget, listOrphanTargets, clearOrphanTargets
                , queryCodeAttr )
import Errors (EntangledError (..))

import Comment (headerComment)
import Document (ReferenceName(..))
import Tangle (ExpandedCode, Annotator, expandedCode, parseMarkdown')
import Stitch (untangle)

type FileTransaction env = Transaction (FileIO env)

newtype Entangled env a = Entangled { unEntangled :: WriterT (FileTransaction env) (RIO env) a }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadThrow
             , MonadReader env, MonadWriter (FileTransaction env) )

instance MonadError EntangledError (Entangled env) where
    throwError = throwM
    catchError x _ = x

testEntangled :: (MonadIO m, MonadReader env m, HasLogFunc env)
              => Entangled env a -> m Bool
testEntangled (Entangled x) = do
    e <- ask
    (_, w) <- runRIO e (runWriterT x)
    runFileIO' $ testTransaction w

runEntangled :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Bool -> Maybe Doc -> Entangled env a -> m a
runEntangled True  _ = runEntangledMachine
runEntangled False h = runEntangledHuman h

runEntangledMachine :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Entangled env a -> m a
runEntangledMachine (Entangled x) = do
    e <- ask
    (r, w) <- runRIO e (runWriterT x)
    runFileIO' $ runTransactionMachine w
    return r

runEntangledHuman :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Maybe Doc -> Entangled env a -> m a
runEntangledHuman h (Entangled x) = do
    e <- ask
    (r, w) <- runRIO e (runWriterT x)
    ts <- timeStamp
    runFileIO' $ runTransaction (h >>= (\h' -> Just $ ts <> " " <> h')) w
    return r

instance (HasLogFunc env) => MonadFileIO (Entangled env) where
    readFile = readFile'
    dump = dump'

    writeFile path text = do
        old_content' <- liftRIO $ try $ runFileIO' $ readFile path
        case (old_content' :: Either IOException Text) of
            Right old_content | old_content == text -> return ()
                              | otherwise           -> actionw
            Left  _                                 -> actionc
        where actionw   = tell $ plan (WriteFile path) (writeFile path text)
              actionc   = tell $ plan (CreateFile path) (writeFile path text)

    deleteFile path     = tell $ plan (DeleteFile path) (deleteFile path)

-- up for deletion
data TangleQuery = TangleFile FilePath | TangleRef Text | TangleAll deriving (Show, Eq)

takeLines :: Text -> Int -> [Text]
takeLines txt n = take n $ drop 1 $ T.lines txt

dropLines :: Text -> Int -> [Text]
dropLines txt n = take 1 lines_ <> drop (n+1) lines_
    where lines_ = T.lines txt

toInt :: Text -> Maybe Int
toInt = readMaybe . T.unpack

tangleRef :: (HasLogFunc env, HasConfig env)
    => ExpandedCode (Entangled env) -> ReferenceName -> Entangled env Text
tangleRef codes name =
    case codes LM.!? name of
        Nothing        -> throwM $ TangleError $ "Reference `" <> tshow name <> "` not found."
        Just t         -> t

tangleFile :: (HasConnection env, HasLogFunc env, HasConfig env)
           => ExpandedCode (Entangled env) -> FilePath -> Entangled env Text
tangleFile codes path = do
    cfg <- view config
    db (queryTargetRef path) >>= \case
        Nothing              -> throwM $ TangleError $ "Target `" <> T.pack path <> "` not found."
        Just (ref, langName) -> do
            content <- tangleRef codes ref
            headerLen <- db (queryCodeAttr ref "header")
            case languageFromName cfg langName of
                Nothing -> throwM $ TangleError $ "Language unknown " <> langName
                Just lang -> return $ maybe (T.unlines [headerComment lang path, content])
                                            (\n -> T.unlines $ takeLines content n <> [headerComment lang path] <> dropLines content n)
                                            (toInt =<< headerLen)

tangle :: (HasConnection env, HasLogFunc env, HasConfig env)
       => TangleQuery -> Annotator (Entangled env) -> Entangled env ()
tangle query annotate = do
    cfg <- view config
    refs <- db (queryReferenceMap cfg)
    let codes = expandedCode annotate refs
    case query of
        TangleRef ref   -> dump =<< tangleRef codes (ReferenceName ref)
        TangleFile path -> dump =<< tangleFile codes path
        TangleAll       -> mapM_ (\f -> writeFile f =<< tangleFile codes f) =<< db listTargetFiles
--

data StitchQuery = StitchFile FilePath | StitchAll

stitchFile :: (HasConnection env, HasLogFunc env, HasConfig env)
       => FilePath -> Entangled env Text
stitchFile path = db (stitchDocument path)

stitch :: (HasConnection env, HasLogFunc env, HasConfig env)
       => StitchQuery -> Entangled env ()
stitch (StitchFile path) = dump =<< stitchFile path
stitch StitchAll = mapM_ (\f -> writeFile f =<< stitchFile f) =<< db listSourceFiles

listTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
            => Entangled env ()
listTargets = dump . T.unlines . map T.pack =<< db listTargetFiles

insertSources :: (HasConnection env, HasLogFunc env, HasConfig env)
              => [FilePath] -> Entangled env ()
insertSources files = do
    logDebug $ display $ "inserting files: " <> tshow files
    mapM_ readDoc files
    where readDoc f = do
            document <- parseMarkdown' f =<< readFile f
            db (insertDocument f document)

insertTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
              => [FilePath] -> Entangled env ()
insertTargets files = do
    logDebug $ display $ "inserting files: " <> tshow files
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
