module Algorithm.AutomataMerge (mergeDfa) where

import Data.Dfa

import Data.IntMap as IMap
import Data.IntSet as ISet
import Data.Map as Map

-- | Merges two DFAs. The states of the first automata won't get renamed. Since it will be difficult
-- to find out which was the initial state of the second DFA, it will be returned as well.
mergeDfa :: (Ord c) => Dfa c -> Dfa c -> (Int, Dfa c)
mergeDfa dfa1 dfa2 =
    let maximumStateDfa1 =
            ISet.findMax (dfaStates dfa1)
        minimumStateDfa2 =
            ISet.findMin (dfaStates dfa2)
    in
        if minimumStateDfa2 > maximumStateDfa1
               then mergeDfa' dfa1 dfa2
               else
                    let newDfa2States =
                            [(maximumStateDfa1 + 1)..]
                        mapping = ISet.toAscList (dfaStates dfa2) `zip` newDfa2States
                        substitution =
                            IMap.fromList mapping
                    in
                        mergeDfa' dfa1 (renameStates substitution dfa2)

-- | Merges the given automata /without looking for overlapping states/.
-- The error state will be the one of the first automata.
mergeDfa' :: (Ord c) => Dfa c -> Dfa c -> (Int, Dfa c)
mergeDfa' dfa1 dfa2 =
    let states =
            dfaErrorState dfa2 `ISet.delete` (dfaStates dfa1 `ISet.union` dfaStates dfa2)
        initialState =
            dfaInitialState dfa1
        finalStates =
            dfaFinalStates dfa1 `ISet.union` dfaFinalStates dfa2
        errorState =
            dfaErrorState dfa1
        errorStateSubstituted :: (Ord c) => Dfa c -> Dfa c
        errorStateSubstituted = renameStates (dfaErrorState dfa2 `IMap.singleton` errorState)
        transitions =
            dfaTransitionFunction dfa1 `Map.union` dfaTransitionFunction (errorStateSubstituted dfa2)
    in
        (dfaInitialState dfa2, Dfa states initialState finalStates errorState transitions)

-- | Rename states of the given automata.
-- /The input is a substitution which must not be overlapping, e.g. map two states to the same state./
renameStates :: (Ord c) => IMap.IntMap Int -> Dfa c -> Dfa c
renameStates substitution dfa =
    let
        -- | Replace the state. If it is not contained, don't replace it.
        subst state =
            IMap.findWithDefault state state substitution
        states =
            ISet.map subst (dfaStates dfa)
        initialState =
            subst (dfaInitialState dfa)
        finalStates =
            ISet.map subst (dfaFinalStates dfa)
        errorState =
            subst (dfaErrorState dfa)
        transitions =
            Map.map subst . Map.mapKeys (\(s, c) -> (subst s, c)) $ dfaTransitionFunction dfa
    in
        Dfa states initialState finalStates errorState transitions
