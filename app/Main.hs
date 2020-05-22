-- ------ language="Haskell" file="app/Main.hs" project://lit/12-main.md
{-# LANGUAGE LambdaCase #-}
module Main where

-- ------ begin <<main-imports>>[0] project://lit/12-main.md
import Prelude hiding (readFile, writeFile)
import RIO (logError, logInfo, display, LogFunc, HasLogFunc, logFuncL, lens, logOptionsHandle, runRIO, withLogFunc, stderr, view)
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
import Control.Monad.Logger (logInfoN)
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
import Database (HasConnection, connection, createTables, db)
import Database.SQLite.Simple
-- ------ end

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
                                    (progDesc "Print an example configuration."))
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
        -- ------ begin <<sub-runners>>[0] project://lit/12-main.md
        CommandDaemon a -> do
            cfg <- view config
            liftIO $ runSession cfg
        -- ------ end
        -- ------ begin <<sub-runners>>[1] project://lit/12-main.md
        CommandConfig -> printExampleConfig
        -- ------ end
        -- ------ begin <<sub-runners>>[2] project://lit/12-main.md
        CommandInsert (InsertArgs SourceFile fs) -> insertSources fs
        CommandInsert (InsertArgs TargetFile fs) -> insertTargets fs
        -- ------ end
        -- ------ begin <<sub-runners>>[3] project://lit/12-main.md
        CommandTangle (TangleArgs {..}) -> do
            cfg <- view config
            let annotate = if tangleDecorate then (annotateComment' cfg) else annotateNaked
            tangle tangleQuery annotate
        -- ------ end
        -- ------ begin <<sub-runners>>[4] project://lit/12-main.md
        CommandStitch (StitchArgs {..}) -> stitch stitchTarget
        -- ------ end
        -- ------ begin <<sub-runners>>[5] project://lit/12-main.md
        CommandList -> listTargets
        -- ------ end
        -- ------ begin <<sub-runners>>[6] project://lit/12-main.md
        CommandClearOrphans -> clearOrphans
        -- ------ end
-- ------ end
-- ------ end
