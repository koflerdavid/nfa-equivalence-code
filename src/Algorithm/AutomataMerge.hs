module Algorithm.AutomataMerge (mergeDfa) where

import Data.Dfa

import Data.IntSet as ISet
import Data.Map as Map

-- | Merges two DFAs. The states of the first automata won't get renamed.
-- A mapping from the old states of the second automata to the new states is returned as well.
-- The mapping is defined only over the old states of the second automata.
mergeDfa :: (Ord c) => Dfa c -> Dfa c -> (Int -> Int, Dfa c)
mergeDfa dfa1 dfa2 =
    let maximumStateDfa1 =
            ISet.findMax (dfaStates dfa1)
        minimumStateDfa2 =
            ISet.findMin (dfaStates dfa2)
        stateOffset =
            maximumStateDfa1 - minimumStateDfa2 + 1
        dfa2' =
            translateDfaStates dfa2 stateOffset
    in
        if minimumStateDfa2 > maximumStateDfa1
               then (id, mergeDfa' dfa1 dfa2)
               else ((+ stateOffset), mergeDfa' dfa1 dfa2')

-- | Merges the given automata /without looking for overlapping states/.
mergeDfa' :: (Ord c) => Dfa c -> Dfa c -> Dfa c
mergeDfa' dfa1 dfa2 =
    let acceptingStates =
            dfaAcceptingStates dfa1 `ISet.union` dfaAcceptingStates dfa2
        transitions =
            dfaTransitions dfa1 `Map.union` dfaTransitions dfa2
    in
        buildDfaUnsafe (ISet.toAscList acceptingStates) (Map.toAscList transitions)
