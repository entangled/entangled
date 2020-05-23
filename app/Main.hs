-- ~\~ language=Haskell filename=app/Main.hs
-- ~\~ begin <<lit/12-main.md|app/Main.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO

-- ~\~ begin <<lit/12-main.md|main-imports>>[0]
import GHC.IO.Encoding
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-imports>>[1]
import Options.Applicative
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-imports>>[2]
import Config
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-imports>>[3]
import Daemon (runSession)
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-imports>>[4]
import Database (HasConnection, connection, createTables, db)
import Database.SQLite.Simple
-- ~\~ end

import Tangle (annotateNaked, annotateComment')
import FileIO (dump)
import Entangled

-- ~\~ begin <<lit/12-main.md|main-options>>[0]
data Args = Args
    { versionFlag :: Bool
    , verboseFlag :: Bool
    , subCommand :: SubCommand }

data SubCommand
    = NoCommand
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[0]
    | CommandDaemon DaemonArgs
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[1]
    | CommandConfig
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[2]
    | CommandInsert InsertArgs
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[3]
    | CommandTangle TangleArgs
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[4]
    | CommandStitch StitchArgs
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[5]
    | CommandList
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[6]
    | CommandClearOrphans
    -- ~\~ end
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[1]
parseNoCommand :: Parser SubCommand
parseNoCommand = pure NoCommand

parseArgs :: Parser Args   {- HLINT ignore parseArgs -}
parseArgs = Args
    <$> switch (long "version" <> short 'v' <> help "Show version information.")
    <*> switch (long "verbose" <> short 'V' <> help "Be very verbose.")
    <*> ( subparser ( mempty
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[0]
          <>  command "daemon" (info parseDaemonArgs ( progDesc "Run the entangled daemon." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[1]
          <> command "config" (info (pure CommandConfig <**> helper)
                                    (progDesc "Print an example configuration."))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[2]
          <> command "insert" (info parseInsertArgs ( progDesc "Insert markdown files into database." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[3]
          <> command "tangle" (info (CommandTangle <$> parseTangleArgs) ( progDesc "Retrieve tangled code." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[4]
          <> command "stitch" (info (CommandStitch <$> parseStitchArgs) ( progDesc "Retrieve stitched markdown." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[5]
          <> command "list" (info (pure CommandList <**> helper) ( progDesc "List generated code files." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[6]
          <> command "clear-orphans" (info (pure CommandClearOrphans <**> helper) ( progDesc "Deletes orphan targets." ))
          -- ~\~ end
        ) <|> parseNoCommand )
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[2]
newtype DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon . DaemonArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[3]
data FileType = SourceFile | TargetFile

data InsertArgs = InsertArgs
    { insertType :: FileType
    , insertFiles :: [FilePath] }

parseFileType :: Parser FileType
parseFileType = flag' SourceFile (long "source" <> short 's' <> help "insert markdown source file")
            <|> flag' TargetFile (long "target" <> short 't' <> help "insert target code file")

parseInsertArgs :: Parser SubCommand
parseInsertArgs = CommandInsert <$> (InsertArgs
    <$> parseFileType
    <*> many (argument str (metavar "FILES..."))
    <**> helper)
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[4]
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
        <|> flag' TangleAll (long "all" <> short 'a' <> help "tangle all and write to disk" ))
    <*> switch (long "decorate" <> short 'd' <> help "Decorate with stitching comments.")
    <**> helper
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[5]
newtype StitchArgs = StitchArgs
    { stitchTarget :: FilePath
    } deriving (Show)

parseStitchArgs :: Parser StitchArgs
parseStitchArgs = StitchArgs
    <$> argument str ( metavar "TARGET" )
    <**> helper
-- ~\~ end

main :: IO ()
main = do
    -- ~\~ begin <<lit/12-main.md|main-set-encoding>>[0]
    setLocaleEncoding utf8
    -- ~\~ end
    run =<< execParser args
    where args = info (parseArgs <**> helper)
            (  fullDesc
            <> progDesc "Automatically tangles and untangles 'FILES...'."
            <> header   "enTangleD -- daemonised literate programming"
            )

-- ~\~ begin <<lit/12-main.md|main-run>>[0]
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
    | versionFlag       = runWithEnv False (dump "Entangled 1.0.0\n")
    | otherwise         = runWithEnv verboseFlag (runSubCommand subCommand)

runWithEnv :: Bool -> Entangled Env a -> IO a
runWithEnv verbose x = do
    cfg <- readLocalConfig
    dbPath <- getDatabasePath cfg
    logOptions <- setLogVerboseFormat True . setLogUseColor True
               <$> logOptionsHandle stderr verbose
    withLogFunc logOptions (\logFunc
        -> withConnection dbPath (\conn
            -> runRIO (Env conn cfg logFunc) (runEntangled Nothing x)))

runSubCommand :: (HasConfig env, HasLogFunc env, HasConnection env)
              => SubCommand -> Entangled env ()
runSubCommand sc = do
    db createTables
    case sc of
        NoCommand -> return ()
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[0]
        CommandDaemon DaemonArgs {..} -> runSession inputFiles
        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[1]
        CommandConfig -> printExampleConfig
        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[2]
        CommandInsert (InsertArgs SourceFile fs) -> insertSources fs
        CommandInsert (InsertArgs TargetFile fs) -> insertTargets fs
        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[3]
        CommandTangle TangleArgs {..} -> do
            cfg <- view config
            let annotate = if tangleDecorate then annotateComment' cfg else annotateNaked
            tangle tangleQuery annotate
        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[4]
        CommandStitch StitchArgs {..} -> stitch (StitchFile stitchTarget)
        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[5]
        CommandList -> listTargets
        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[6]
        CommandClearOrphans -> clearOrphans
        -- ~\~ end
-- ~\~ end
-- ~\~ end
