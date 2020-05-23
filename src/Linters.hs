-- ~\~ language=Haskell filename=src/Linters.hs
-- ~\~ begin <<lit/a5-linting.md|src/Linters.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Linters where

import RIO
import qualified RIO.Text as T
import RIO.List (repeat, zip3)
import RIO.Set (elems)
import qualified RIO.Set as S
import qualified RIO.Map as M

import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G

import Database.SQLite.Simple (query_, query, fromOnly, Only(..))

import Entangled
import Document (ReferenceMap, ReferenceName(..), codeBlocksByName, CodeBlock(..), referenceNames)
import Database (HasConnection, db, queryReferenceMap, getConnection)
import Config (HasConfig, config)
import Tangle (parseCode, CodeLine(..))
import FileIO

referencesInFragment :: ReferenceMap -> ReferenceName -> [ReferenceName]
referencesInFragment refs r =
    concatMap (mapMaybe getReference . parseCode r . codeSource)
              (codeBlocksByName refs r)
    where getReference (NowebReference s _) = Just s
          getReference _                    = Nothing

referenceGraph :: (HasConnection env, HasLogFunc env, HasConfig env)
               => Entangled env (Graph, Vertex -> ((), ReferenceName, [ReferenceName]), ReferenceName -> Maybe Vertex)
referenceGraph = do
    cfg <- view config
    refs <- db (queryReferenceMap cfg)
    let names = elems $ referenceNames refs
    return $ G.graphFromEdges $ zip3 (repeat ()) names (map (referencesInFragment refs) names)

dumpToGraphViz :: (HasConnection env, HasLogFunc env, HasConfig env)
               => Entangled env ()
dumpToGraphViz = do
    (graph, vertexToNode, _) <- referenceGraph
    let graphvizEdges = T.unlines $ map edgeToGV $ G.edges graph
        nodeToKey (_, ReferenceName b, _) = "\"" <> b <> "\""
        edgeToGV (a, b) = nodeToKey (vertexToNode a) <> " -> " <> nodeToKey (vertexToNode b)
    dump $ "digraph {\n" <> graphvizEdges <> "}"

listUnusedFragments :: (HasConnection env, HasLogFunc env, HasConfig env)
                    => Entangled env ()
listUnusedFragments = do
    (graph, vertexToNode, keyToVertex) <- referenceGraph
    conn <- getConnection
    roots <- mapMaybe (keyToVertex . ReferenceName . fromOnly)
          <$> liftIO (query_ conn "select `codename` from `targets` \
                                  \where `document` is not null" :: IO [Only Text])
    let reach = foldMap (S.fromList . G.reachable graph) roots
        unused = S.fromList (G.vertices graph) S.\\ reach
        nodeToKey (_, (ReferenceName b), _) = b
        getSource name = liftIO $ query conn "select `codes`.`name`,`documents`.`filename` from `codes` \
                                             \inner join `documents` on `codes`.`document` is `documents`.`id`\
                                             \where `codes`.`name` is ? and `codes`.`ordinal` is 0" (Only name)
    codes <- foldMapM getSource (map (nodeToKey . vertexToNode) (S.toList unused))
    dump $ T.unlines $ map (\(name, src) -> name <> " in " <> src) codes

linters :: (HasConnection env, HasLogFunc env, HasConfig env)
        => Map Text (Entangled env ())
linters = M.fromList
    [ ("dumpToGraphViz", dumpToGraphViz)
    , ("listUnusedFragments", listUnusedFragments) ]

lint :: (HasConnection env, HasLogFunc env, HasConfig env)
     => [Text] -> Entangled env ()
lint = sequence_ . mapMaybe (linters M.!?)
-- ~\~ end
