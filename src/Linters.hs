-- ~\~ language=Haskell filename=src/Linters.hs
-- ~\~ begin <<lit/a5-linting.md|src/Linters.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Linters where

import RIO
import RIO.Writer (MonadWriter, WriterT(..), execWriterT, tell)
import qualified RIO.Text as T
import RIO.List (repeat, zip3)
import RIO.Set (elems)
import qualified RIO.Set as S
import qualified RIO.Map as M

import Data.Graph (Graph, Vertex, SCC(..))
import qualified Data.Graph as G

import Database.SQLite.Simple (query_, query, fromOnly, Only(..))

-- import Entangled
import Document (ReferenceMap, ReferenceName(..), codeBlocksByName, CodeBlock(..), referenceNames)
import Database (HasConnection, db, queryReferenceMap, getConnection)
import Daemon (Session)
import Config (HasConfig, config)
import Tangle (parseCode, CodeLine(..))
-- import FileIO
import qualified RIO.ByteString as B

data LinterOutput = LinterOutput
    { linterErr :: Bool
    , linterWho :: Text
    , linterMsg :: Text
    } deriving (Show)

newtype Linter env a = Linter { unLinter :: WriterT [LinterOutput] (RIO env) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadReader env, MonadWriter [LinterOutput])

data LinterResult = LinterFail [LinterOutput] | LinterSuccess deriving (Show)
instance Semigroup LinterResult where
    LinterFail a <> LinterFail b = LinterFail (a <> b)
    LinterSuccess <> b = b
    a <> LinterSuccess = a
instance Monoid LinterResult where
    mempty = LinterSuccess

dump :: (MonadIO m) => Text -> m ()
dump text = B.hPutStr stdout (T.encodeUtf8 text)

runLinter :: (HasLogFunc env) => Linter env () -> RIO env LinterResult
runLinter (Linter l) = do
    result <- execWriterT l
    case filter linterErr result of
      []  -> return LinterSuccess
      err -> return $ LinterFail err

linterWarn :: (HasLogFunc env) => Text -> Text -> Linter env ()
linterWarn who msg = do
    logWarn $ display who <> ": " <> display msg
    tell [LinterOutput False who msg]

linterError :: (HasLogFunc env) => Text -> Text -> Linter env ()
linterError who msg = do
    logError $ display who <> ": " <> display msg
    tell [LinterOutput True who msg]

referencesInFragment :: ReferenceMap -> ReferenceName -> [ReferenceName]
referencesInFragment refs r =
    concatMap (mapMaybe getReference . parseCode r . codeSource)
              (codeBlocksByName refs r)
    where getReference (NowebReference s _) = Just s
          getReference _                    = Nothing

referenceGraph :: (MonadIO m, MonadReader env m, HasConnection env, HasLogFunc env, HasConfig env)
               => m (Graph, Vertex -> ((), ReferenceName, [ReferenceName]), ReferenceName -> Maybe Vertex)
referenceGraph = do
    cfg <- view config
    refs <- db (queryReferenceMap cfg)
    let names = elems $ referenceNames refs
    return $ G.graphFromEdges $ zip3 (repeat ()) names (map (referencesInFragment refs) names)

referenceSCC :: (MonadIO m, MonadReader env m, HasConnection env, HasLogFunc env, HasConfig env)
             => m [SCC ReferenceName]
referenceSCC = do
    cfg <- view config
    refs <- db (queryReferenceMap cfg)
    let names = elems $ referenceNames refs
    return $ G.stronglyConnComp $ zip3 names names (map (referencesInFragment refs) names)

checkCycles :: (HasConnection env, HasLogFunc env, HasConfig env)
            => Linter env ()
checkCycles = do
    scc <- referenceSCC
    unless (all isAcyclic scc) $ do
        let cycles = map (\c -> "cycle found: " <> T.intercalate " -> " c <> " ⤾ ")
                         $ mapMaybe cyclicName scc
        linterError "checkCycles" (T.unlines cycles)

    where isAcyclic (CyclicSCC _) = False
          isAcyclic _             = True
          cyclicName (CyclicSCC ref) = Just (map unReferenceName ref)
          cyclicName _               = Nothing

dumpToGraphViz :: (HasConnection env, HasLogFunc env, HasConfig env)
               => Linter env ()
dumpToGraphViz = do
    (graph, vertexToNode, _) <- referenceGraph
    let graphvizEdges = T.unlines $ map edgeToGV $ G.edges graph
        nodeToKey (_, ReferenceName b, _) = "\"" <> b <> "\""
        edgeToGV (a, b) = nodeToKey (vertexToNode a) <> " -> " <> nodeToKey (vertexToNode b)
    dump $ "digraph {\n" <> graphvizEdges <> "}"
    return ()

listUnusedFragments :: (HasConnection env, HasLogFunc env, HasConfig env)
                    => Linter env ()
listUnusedFragments = do
    (graph, vertexToNode, keyToVertex) <- referenceGraph
    conn <- getConnection
    roots <- mapMaybe (keyToVertex . ReferenceName . fromOnly)
          <$> liftIO (query_ conn "select `codename` from `targets` \
                                  \where `document` is not null" :: IO [Only Text])
    let reach = foldMap (S.fromList . G.reachable graph) roots
        unused = S.fromList (G.vertices graph) S.\\ reach
        nodeToKey (_, ReferenceName b, _) = b
        getSource name = liftIO $ query conn "select `codes`.`name`,`documents`.`filename` from `codes` \
                                             \inner join `documents` on `codes`.`document` is `documents`.`id`\
                                             \where `codes`.`name` is ? and `codes`.`ordinal` is 0" (Only name)
    codes <- foldMapM getSource (map (nodeToKey . vertexToNode) (S.toList unused))
    dump $ T.unlines $ map (\(name, src) -> name <> " in " <> src) codes
    return ()

linters :: (HasConnection env, HasLogFunc env, HasConfig env)
        => Map Text (Linter env ())
linters = M.fromList
    [ ("dumpToGraphViz", dumpToGraphViz)
    , ("checkCycles", checkCycles)
    , ("listUnusedFragments", listUnusedFragments) ]

allLinters :: [Text]
allLinters  = M.keys (linters :: Map Text (Linter Session ()))

lint :: (HasConnection env, HasLogFunc env, HasConfig env)
     => [Text] -> RIO env LinterResult
lint [] | allLinters /= [] = lint allLinters -- use all available linters
lint args = mconcat <$> mapM runLinter (mapMaybe (linters M.!?) args)
-- ~\~ end
