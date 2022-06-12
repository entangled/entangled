-- ~\~ language=Haskell filename=app/Main.hs
-- ~\~ begin <<lit/12-main.md|app/Main.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import RIO.Text (unwords, unpack)

import Prelude (putStrLn)
import Paths_entangled
import Data.Version (showVersion)

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
import Database (HasConnection, createTables, db)
-- import Comment (annotateNaked)
-- ~\~ end

import Entangled
import Linters
import qualified Commands.Common as Common
import qualified Commands.Config
import qualified Commands.Insert
import qualified Commands.List
import qualified Commands.Tangle
import qualified Commands.Stitch

-- ~\~ begin <<lit/12-main.md|main-options>>[0]
data SubCommand
    = NoCommand
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[0]
    | CommandDaemon DaemonArgs
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[1]
    | CommandConfig Commands.Config.Args
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[2]
    | CommandInsert Commands.Insert.Args
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[3]
    | CommandTangle Commands.Tangle.Args
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[4]
    | CommandStitch Commands.Stitch.Args
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[5]
    | CommandList Commands.List.Args
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[6]
    | CommandLint LintArgs
    -- ~\~ end
    -- ~\~ begin <<lit/12-main.md|sub-commands>>[7]
    | CommandClearOrphans
    -- ~\~ end
    deriving (Show, Eq)
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[1]
parseNoCommand :: Parser SubCommand
parseNoCommand = pure NoCommand

parseSubCommand :: Parser SubCommand   {- HLINT ignore parseArgs -}
parseSubCommand = ( subparser ( mempty
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[0]
          <>  command "daemon" (info parseDaemonArgs ( progDesc "Run the entangled daemon." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[1]
          <> command "config" (info (CommandConfig <$> Commands.Config.parseArgs)
                                    (progDesc "Print an example configuration."))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[2]
          <> command "insert" (info (CommandInsert <$> Commands.Insert.parseArgs)
                                    ( progDesc "Insert markdown files into database." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[3]
          <> command "tangle" (info (CommandTangle <$> Commands.Tangle.parseArgs) ( progDesc "Retrieve tangled code." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[4]
          <> command "stitch" (info (CommandStitch <$> Commands.Stitch.parseArgs) ( progDesc "Retrieve stitched markdown." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[5]
          <> command "list" (info (CommandList <$> Commands.List.parseArgs)
                                  (progDesc "List generated code files." ))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[6]
          <> command "lint" (info (CommandLint <$> parseLintArgs) ( progDesc ("Lint input on potential problems. Available linters: " <> RIO.Text.unpack (RIO.Text.unwords allLinters))))
          -- ~\~ end
          -- ~\~ begin <<lit/12-main.md|sub-parsers>>[7]
          <> command "clear-orphans" (info (pure CommandClearOrphans <**> helper) ( progDesc "Deletes orphan targets." ))
          -- ~\~ end
        ) <|> parseNoCommand )
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[2]
newtype DaemonArgs = DaemonArgs
    { inputFiles  :: [String]
    } deriving (Show, Eq)

parseDaemonArgs :: Parser SubCommand
parseDaemonArgs = CommandDaemon . DaemonArgs
    <$> many (argument str (metavar "FILES..."))
    <**> helper
-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[3]

-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[4]

-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[5]

-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[6]

-- ~\~ end
-- ~\~ begin <<lit/12-main.md|main-options>>[7]
newtype LintArgs = LintArgs
    { lintFlags :: [Text]
    } deriving (Show, Eq)

parseLintArgs :: Parser LintArgs
parseLintArgs = LintArgs
    <$> many (argument str (metavar "LINTERS"))
    <**> helper
-- ~\~ end

main :: IO ()
main = do
    -- ~\~ begin <<lit/12-main.md|main-set-encoding>>[0]
    setLocaleEncoding utf8
    -- ~\~ end
    run =<< execParser args
    where args = info (Common.parseArgs parseSubCommand <**> helper)
            (  fullDesc
            <> progDesc "Automatically tangles and untangles 'FILES...'."
            <> header   "Entangled -- daemonised literate programming"
            )

-- ~\~ begin <<lit/12-main.md|main-run>>[0]
run :: Common.Args SubCommand -> IO ()
run (Common.Args True _ _ _ _ _) = putStrLn $ showVersion version
run args@Common.Args{..} = 
    case subArgs of
      CommandConfig x -> Commands.Config.run (args {Common.subArgs = x})
      CommandInsert x -> Common.withEnv args $ Common.withEntangled args $ Commands.Insert.run x
      CommandList x   -> Common.withEnv args $ Commands.List.run (args {Common.subArgs = x})
      CommandTangle x -> Common.withEnv args $ Common.withEntangled args $ Commands.Tangle.run x
      CommandStitch x -> Common.withEnv args $ Common.withEntangled args $ Commands.Stitch.run x
      _               -> Common.withEnv args $ Common.withEntangled args (runSubCommand subArgs)

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
        CommandConfig _ -> printExampleConfig
        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[2]

        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[3]

        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[4]

        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[5]

        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[6]
        CommandLint LintArgs {..} -> void $ liftRIO $ lint lintFlags
        -- ~\~ end
        -- ~\~ begin <<lit/12-main.md|sub-runners>>[7]
        CommandClearOrphans -> clearOrphans
        -- ~\~ end
        _    -> return ()
-- ~\~ end
-- ~\~ end
