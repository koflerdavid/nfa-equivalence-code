{- |
Module:      Algorithm.AutomataMerge

Description: This module provides an algorithm to merge two DFAs.
Copyright:   (C) David Kofler
License:     Please see LICENCE file of project

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)

Provides an algorithm to merge two DFAs.
It is crucial to avoid overlaps between state names because the goal is
to transform the two passed DFAs into subautomata of the resulting DFA.
-}

module Algorithm.AutomataMerge
    ( mergeDfa
    ) where

import Data.Dfa

import Data.IntSet as ISet
import Data.Map    as Map
import Data.Maybe  ( fromJust )

-- | Merges two DFAs. The states of the first automata won't get renamed.
-- A mapping from the old states of the second automata to the new states is returned as well.
-- The mapping is defined only over the old states of the second automata.
mergeDfa :: (Ord c) => Dfa c -> Dfa c -> (DfaState -> DfaState, Dfa c)
mergeDfa dfa1 dfa2
    | isEmpty dfa1 = (id, dfa2)
    | isEmpty dfa2 = (id, dfa1)
    | minimumStateDfa2 > maximumStateDfa1 =  (id, mergeDfaUnsafe dfa1 dfa2)
    | otherwise =
        let mergedDfa = mergeDfaUnsafe dfa1 dfa2' in
               (shiftStateBy stateOffset dfa2', mergedDfa)
  where
    maximumStateDfa1 = ISet.findMax (dfaStates dfa1)
    minimumStateDfa2 = ISet.findMin (dfaStates dfa2)
    stateOffset = maximumStateDfa1 - minimumStateDfa2 + 1
    dfa2' = translateDfaStates dfa2 stateOffset

    shiftStateBy :: Int -> Dfa c -> DfaState -> DfaState
    shiftStateBy offset dfa = maybe dfaErrorState (fromJust . toDfaState dfa . (+ offset)) . toStateNumber

-- | Merges the given automata /without looking for overlapping states/.
mergeDfaUnsafe :: (Ord c) => Dfa c -> Dfa c -> Dfa c
mergeDfaUnsafe dfa1 dfa2 =
    buildDfaUnsafe
        (ISet.toAscList allAcceptingStates)
        (Map.toAscList allTransitions)
  where
    allAcceptingStates =
        dfaAcceptingStates dfa1 `ISet.union` dfaAcceptingStates dfa2
    allTransitions = dfaTransitions dfa1 `Map.union` dfaTransitions dfa2

-- | Checks for the simplest case when a DFA is empty.
-- This is the case when there are no transitions and no accepting states.
-- In that case, the automaton can be considered to be a neutral element redarding automata merging.
-- Neither of these conditions is sufficient by themselves since an empty automaton can
-- still have some meaning thanks to some accepting states.
-- Also, an automaton with no accepting states is still a valid transition system.
isEmpty :: Dfa c -> Bool
isEmpty dfa = ISet.null (dfaAcceptingStates dfa) && Map.null (dfaTransitions dfa)
