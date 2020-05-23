# Linting

``` {.haskell file=src/Linters.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Linters where

import RIO
import RIO.List (repeat, zip3)
import RIO.Set (elems)

import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G

import Entangled
import Document (ReferenceMap, ReferenceName(..), codeBlocksByName, CodeBlock(..), referenceNames)
import Database (HasConnection, db, queryReferenceMap)
import Config (HasConfig, config)
import Tangle (parseCode, CodeLine(..))

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

listUnusedFragments :: (HasConnection env, HasLogFunc env, HasConfig env)
                    => Entangled env ()
listUnusedFragments = return ()

lint :: (HasConnection env, HasLogFunc env, HasConfig env)
     => [Text] -> Entangled env ()
lint _ = return ()
```
