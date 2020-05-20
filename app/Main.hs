-- ------ language="Haskell" file="app/Main.hs" project://lit/12-main.md
module Main where

-- ------ begin <<main-imports>>[0] project://lit/12-main.md
import Prelude hiding (readFile, writeFile)
-- ------ begin <<import-text>>[0] project://lit/01-entangled.md
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import qualified Data.Text.IO as T.IO
import qualified Data.Map.Lazy as LM
import Data.List (sortOn)

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Catch
import Control.Monad.Logger
import System.Directory
import System.FilePath
-- ------ end
-- ------ begin <<main-imports>>[1] project://lit/12-main.md
import GHC.IO.Encoding
-- ------ end
-- ------ begin <<main-imports>>[2] project://lit/12-main.md
import Options.Applicative
-- ------ end
-- ------ begin <<main-imports>>[3] project://lit/12-main.md
import Config
-- ------ end
-- ------ begin <<main-imports>>[4] project://lit/12-main.md
import Daemon (runSession)
-- ------ end
-- ------ begin <<main-imports>>[5] project://lit/12-main.md
import qualified Dhall
-- ------ end
-- ------ begin <<main-imports>>[6] project://lit/12-main.md
import Database
import Database.SQLite.Simple
-- ------ end
-- ------ begin <<main-imports>>[7] project://lit/12-main.md
import Stitch (stitch)
-- ------ end

import Comment
import Document
import Select (select)
import System.Exit
import Tangle (parseMarkdown, expandedCode, annotateNaked, annotateComment')
import TextUtil
import FileIO

-- ------ begin <<main-options>>[0] project://lit/12-main.md
data Args = Args
    { versionFlag :: Bool
    , subCommand :: SubCommand }

data SubCommand
    = NoCommand
    -- ------ begin <<sub-commands>>[0] project://lit/12-main.md
    | CommandDaemon DaemonArgs
    -- ------ end
    -- ------ begin <<sub-commands>>[1] project://lit/12-main.md
    | CommandConfig
    -- ------ end
    -- ------ begin <<sub-commands>>[2] project://lit/12-main.md
    | CommandInsert InsertArgs
    -- ------ end
    -- ------ begin <<sub-commands>>[3] project://lit/12-main.md
    | CommandTangle TangleArgs
    -- ------ end
    -- ------ begin <<sub-commands>>[4] project://lit/12-main.md
    | CommandStitch StitchArgs
    -- ------ end
    -- ------ begin <<sub-commands>>[5] project://lit/12-main.md
    | CommandList
    -- ------ end
    -- ------ begin <<sub-commands>>[6] project://lit/12-main.md
    | CommandClearOrphans
    -- ------ end
-- ------ end
-- ------ begin <<main-options>>[1] project://lit/12-main.md
parseNoCommand :: Parser SubCommand
parseNoCommand = pure NoCommand

parseArgs :: Parser Args
parseArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> ( subparser ( mempty
          -- ------ begin <<sub-parsers>>[0] project://lit/12-main.md
          <>  command "daemon" (info parseDaemonArgs ( progDesc "Run the entangled daemon." ))
          -- ------ end
          -- ------ begin <<sub-parsers>>[1] project://lit/12-main.md
          <> command "config" (info (pure CommandConfig <**> helper)
                                    (progDesc "Print the default configuration."))
          -- ------ end
          -- ------ begin <<sub-parsers>>[2] project://lit/12-main.md
          <> command "insert" (info parseInsertArgs ( progDesc "Insert markdown files into database." ))
          -- ------ end
          -- ------ begin <<sub-parsers>>[3] project://lit/12-main.md
          <> command "tangle" (info (CommandTangle <$> parseTangleArgs) ( progDesc "Retrieve tangled code." ))
          -- ------ end
          -- ------ begin <<sub-parsers>>[4] project://lit/12-main.md
          <> command "stitch" (info (CommandStitch <$> parseStitchArgs) ( progDesc "Retrieve stitched markdown." ))
          -- ------ end
          -- ------ begin <<sub-parsers>>[5] project://lit/12-main.md
          <> command "list" (info (pure CommandList <**> helper) ( progDesc "List generated code files." ))
          -- ------ end
          -- ------ begin <<sub-parsers>>[6] project://lit/12-main.md
          <> command "clear-orphans" (info (pure CommandClearOrphans <**> helper) ( progDesc "Deletes orphan targets." ))
          -- ------ end
        ) <|> parseNoCommand )
-- ------ end
-- ------ begin <<main-options>>[2] project://lit/12-main.md
data DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon <$> DaemonArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
-- ------ end
-- ------ begin <<main-options>>[3] project://lit/12-main.md
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
-- ------ end
-- ------ begin <<main-options>>[4] project://lit/12-main.md
data TangleQuery = TangleFile FilePath | TangleRef Text | TangleAll deriving (Show)

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
-- ------ end
-- ------ begin <<main-options>>[5] project://lit/12-main.md
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
    -- ------ begin <<main-set-encoding>>[0] project://lit/12-main.md
    setLocaleEncoding utf8
    -- ------ end
    run =<< execParser args
    where args = info (parseArgs <**> helper)
            (  fullDesc
            <> progDesc "Automatically tangles and untangles 'FILES...'."
            <> header   "enTangleD -- daemonised literate programming"
            )

-- ------ begin <<main-run>>[0] project://lit/12-main.md
run :: Args -> IO ()
run Args{..}
    | versionFlag       = putStrLn "Entangled 1.0.0"
    | otherwise         = do
        config <- configStack
        case subCommand of
            NoCommand -> return ()
            -- ------ begin <<sub-runners>>[0] project://lit/12-main.md
            CommandDaemon a -> runSession config
            -- ------ end
            -- ------ begin <<sub-runners>>[1] project://lit/12-main.md
            CommandConfig -> T.IO.putStrLn "NYI" -- T.IO.putStrLn $ Toml.encode configCodec config
            -- ------ end
            -- ------ begin <<sub-runners>>[2] project://lit/12-main.md
            CommandInsert (InsertArgs SourceFile fs) -> runFileIO $ runInsertSources config fs
            CommandInsert (InsertArgs TargetFile fs) -> runFileIO $ runInsertTargets config fs
            -- ------ end
            -- ------ begin <<sub-runners>>[3] project://lit/12-main.md
            CommandTangle a -> runFileIO $ runTangle config a
            -- ------ end
            -- ------ begin <<sub-runners>>[4] project://lit/12-main.md
            CommandStitch a -> runFileIO $ runStitch config a
            -- ------ end
            -- ------ begin <<sub-runners>>[5] project://lit/12-main.md
            CommandList -> runFileIO $ runList config
            -- ------ end
            -- ------ begin <<sub-runners>>[6] project://lit/12-main.md
            CommandClearOrphans -> runFileIO $ runClearOrphans config
            -- ------ end
-- ------ end
-- ------ begin <<main-run>>[1] project://lit/12-main.md
runTangle :: Config -> TangleArgs -> FileIO ()
runTangle cfg TangleArgs{..} = do
    dbPath <- getDatabasePath cfg
    withSQL dbPath $ do
        createTables
        refs <- queryReferenceMap cfg
        let annotate = if tangleDecorate then annotateComment' cfg else annotateNaked
            codes = expandedCode annotate refs
            tangleRef tgt = case codes LM.!? tgt of
                Nothing -> throwM $ TangleError $ "Reference `" <> tshow tgt <> "` not found."
                Just (Left e) -> throwM $ TangleError $ tshow e
                Just (Right t) -> return t
            tangleFile f = queryTargetRef f >>= \case
                Nothing -> throwM $ TangleError $ "Target `" <> T.pack f <> "` not found."
                Just (ref, langName) -> do
                    content <- tangleRef ref
                    case languageFromName cfg langName of
                        Nothing -> throwM $ TangleError $ "Language unknown " <> langName
                        Just lang -> return $ T.unlines [headerComment lang f, content]

        case tangleQuery of
            TangleRef tgt -> tangleRef (ReferenceName tgt) >>= (\x -> liftIO $ T.IO.putStr x)
            TangleFile f  -> tangleFile f >>= (\x -> liftIO $ T.IO.putStr x)
            TangleAll -> do
                fs <- listTargetFiles
                mapM_ (\f -> tangleFile f >>= (runFileIO . writeFile f)) fs
-- ------ end
-- ------ begin <<main-run>>[2] project://lit/12-main.md
runStitch :: Config -> StitchArgs -> FileIO ()
runStitch config StitchArgs{..} = do
    dbPath <- getDatabasePath config
    text <- withSQL dbPath $ do
        createTables
        stitchDocument stitchTarget
    liftIO $ T.IO.putStrLn text
-- ------ end
-- ------ begin <<main-run>>[3] project://lit/12-main.md
runList :: Config -> FileIO ()
runList cfg = do
    dbPath <- getDatabasePath cfg
    lst <- withSQL dbPath $ do
        createTables
        listTargetFiles
    liftIO $ T.IO.putStrLn $ unlines' $ map T.pack lst
-- ------ end
-- ------ begin <<main-run>>[4] project://lit/12-main.md
runInsertSources :: Config -> [FilePath] -> FileIO ()
runInsertSources cfg files = do
    dbPath <- getDatabasePath cfg
    logInfoN $ "inserting files: " <> tshow files
    withSQL dbPath $ createTables >> mapM_ readDoc files
    where readDoc f = do
            doc <- runReaderT (runFileIO (readFile f) >>= parseMarkdown f) cfg
            case doc of
                Left e -> liftIO $ T.IO.putStrLn ("error: " <> tshow e)
                Right d -> insertDocument f d

deduplicateRefs :: [ReferencePair] -> SQL [ReferencePair]
deduplicateRefs refs = dedup sorted
    where sorted = sortOn fst refs
          dedup [] = return []
          dedup [x1] = return [x1]
          dedup ((ref1, code1@CodeBlock{codeSource=s1}) : (ref2, code2@CodeBlock{codeSource=s2}) : xs)
                | ref1 /= ref2 = ((ref1, code1) :) <$> dedup ((ref2, code2) : xs)
                | s1 == s2     = dedup ((ref1, code1) : xs)
                | otherwise    = do
                    old_code <- queryCodeSource ref1
                    case old_code of
                        Nothing -> throwM $ StitchError $ "ambiguous update: " <> tshow ref1 <> " not in database."
                        Just c  -> select (throwM $ StitchError $ "ambiguous update to " <> tshow ref1)
                                    [(s1 == c && s2 /= c, dedup ((ref2, code2) : xs))
                                    ,(s1 /= c && s2 == c, dedup ((ref1, code1) : xs))]

runInsertTargets :: Config -> [FilePath] -> FileIO ()
runInsertTargets cfg files = do
    dbPath <- getDatabasePath cfg
    logInfoN $ "inserting files: " <> tshow files
    withSQL dbPath $ createTables >> mapM_ readTgt files
    where readTgt f = do
            refs' <- runReaderT (runFileIO (readFile f) >>= stitch f) cfg
            case refs' of
                Left err -> logErrorN $ "Error loading '" <> T.pack f <> "': " <> formatError err
                Right refs -> updateTarget =<< deduplicateRefs refs
-- ------ end
-- ------ begin <<main-run>>[5] project://lit/12-main.md
runClearOrphans :: Config -> FileIO ()
runClearOrphans cfg = do
    dbPath <- getDatabasePath cfg
    lst <- withSQL dbPath $ do
        createTables
        r <- listOrphanTargets
        clearOrphanTargets
        return r
    mapM_ deleteFile lst
-- ------ end
-- ------ end
