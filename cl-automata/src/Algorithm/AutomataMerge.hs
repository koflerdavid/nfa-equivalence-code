module Algorithm.AutomataMerge
    ( mergeDfa
    ) where

import Data.Dfa

import Data.IntSet as ISet
import Data.Map    as Map

-- | Merges two DFAs. The states of the first automata won't get renamed.
-- A mapping from the old states of the second automata to the new states is returned as well.
-- The mapping is defined only over the old states of the second automata.
mergeDfa :: (Ord c) => Dfa c -> Dfa c -> (Int -> Int, Dfa c)
mergeDfa dfa1 dfa2 =
    if minimumStateDfa2 > maximumStateDfa1
        then (id, mergeDfaUnsafe dfa1 dfa2)
        else ((+ stateOffset), mergeDfaUnsafe dfa1 dfa2')
  where
    maximumStateDfa1 = ISet.findMax (dfaStates dfa1)
    minimumStateDfa2 = ISet.findMin (dfaStates dfa2)
    stateOffset = maximumStateDfa1 - minimumStateDfa2 + 1
    dfa2' = translateDfaStates dfa2 stateOffset

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
