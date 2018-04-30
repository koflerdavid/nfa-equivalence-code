{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module:      Data.FiniteAutomaton.Format.DotGraph
Description: Converts finite automata to Graphviz dotgraphs.
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)
-}
module Data.FiniteAutomaton.Format.DotGraph
    ( DotGraphConfig(..)
    , defaultDotGraphConfig
    , toDefaultDotGraph
    , toDotGraph
    , toDotGraphWithStatePrinter
    ) where

import           Data.FiniteAutomaton

import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Canonical
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import qualified Data.Text.Lazy                    as TL

-- | Allows to configure aspects of how the automaton will be rendered.
data DotGraphConfig q = DotGraphConfig
        -- | The function used to retrieve a description of an automaton state.
    { dgcStatePrinter :: q -> TL.Text
    }

-- | For automata whose states have a `Show` instance, this default configuration can be used.
defaultDotGraphConfig :: Show q => DotGraphConfig q
defaultDotGraphConfig = DotGraphConfig {dgcStatePrinter = TL.pack . show}

-- | Uses `defaultDotGraphConfig` to convert the automaton into a dotgraph.
toDefaultDotGraph ::
       forall m q a. (Ord q, Show a, Show q, FiniteAutomaton m q a Bool)
    => m
    -> DotGraph Int
toDefaultDotGraph = toDotGraph defaultDotGraphConfig

-- | Allows to specify a function to retrieve the text representation of an automaton state.
toDotGraphWithStatePrinter ::
       forall m q a. (Ord q, Show a, FiniteAutomaton m q a Bool)
    => (q -> TL.Text)
    -> m
    -> DotGraph Int
toDotGraphWithStatePrinter statePrinter = toDotGraph withStatePrinter
  where
    withStatePrinter = DotGraphConfig {dgcStatePrinter = statePrinter}

-- | Converts an automaton to a dotgraph.
-- The specified `DotGraphConfig` value controls aspects of the resulting dotgraph.
-- For now, it works only with automata whose states can be classified as accepting.
toDotGraph ::
       forall m q a. (Ord q, Show a, FiniteAutomaton m q a Bool)
    => DotGraphConfig q
    -> m
    -> DotGraph Int
toDotGraph dotGraphConfig m =
    DotGraph
    { strictGraph = True
    , directedGraph = True
    , graphID = Nothing
    , graphStatements =
          DotStmts
          {attrStmts = globalAttributes, subGraphs = parts, nodeStmts = nodes, edgeStmts = edges}
    }
  where
    globalAttributes = []
    parts = []

    nodes :: [DotNode Int]
    nodes = zipWith mkDotNode [1 ..] . Set.toList . faStates $ m

    nodeMap :: Map.Map q Int
    nodeMap = Map.fromDistinctAscList (zip (Set.toAscList (faStates m)) [1 ..])

    edges :: [DotEdge Int]
    edges =
        (`concatMap` Map.toAscList nodeMap) $ \(p, n) ->
            let transitions = Map.toList $ faTransitions m p
            in concat $
               (`map` transitions) $ \(a, qs) ->
                   map (mkDotEdge n a . (nodeMap Map.!)) $ Set.toList qs

    mkDotEdge :: Int -> a -> Int -> DotEdge Int
    mkDotEdge p a q = DotEdge p q [Label (StrLabel . TL.pack . show $ a)]
    mkDotNode :: Int -> q -> DotNode Int
    mkDotNode n q = DotNode n (stateNodeAttributes q (faOutput m q == True))

    stateNodeAttributes :: q -> Bool -> Attributes
    stateNodeAttributes q False =
        [Label (StrLabel . dgcStatePrinter dotGraphConfig $ q), Shape Ellipse]
    stateNodeAttributes q True =
        [Label (StrLabel . dgcStatePrinter dotGraphConfig $ q), Shape DoubleCircle]
