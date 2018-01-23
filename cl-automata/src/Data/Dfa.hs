{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Dfa
    ( Dfa
    , DfaState
    , dfaAlphabet
    , dfaStates
    , dfaAcceptingStates
    , dfaErrorState
    , dfaTransitions
    , dfaAccepts
    , buildDfa
    , buildDfaUnsafe
    , translateDfaStates
    , dfaStep
    , runDfa
    ) where

import           Data.FiniteAutomaton

import qualified Data.Foldable        as Foldable
import qualified Data.IntSet          as ISet
import qualified Data.Map             as Map
import qualified Data.Set             as Set

type DfaState = Maybe Int

data Dfa c =
    Dfa ISet.IntSet
        (Map.Map (Int, c) Int)
    deriving (Eq, Show)

-- | Computes the alphabet of the DFA.
-- This is the set of all characters for which any transition is defined.
dfaAlphabet :: (Ord c) => Dfa c -> Set.Set c
dfaAlphabet (Dfa _ transitions) = Set.map snd . Map.keysSet $ transitions

-- | Computes the states used by this DFA. This is the set of all accepting states and the states
-- which take part in any transition. /The error state will not be returned/
dfaStates :: Dfa c -> ISet.IntSet
dfaStates (Dfa acceptingStates transitions) =
    acceptingStates `ISet.union` transitionStates
  where
    transitionStates = Map.foldMapWithKey comb transitions
    comb (p, _) q = ISet.fromList [p, q]

dfaAcceptingStates :: Dfa c -> ISet.IntSet
dfaAcceptingStates (Dfa acceptingStates _) = acceptingStates

dfaErrorState :: Maybe Int
dfaErrorState = Nothing

dfaTransitions :: Dfa c -> Map.Map (Int, c) Int
dfaTransitions (Dfa _ transitions) = transitions

dfaAccepts :: Dfa c -> DfaState -> Bool
dfaAccepts (Dfa acceptingStates _) (Just q) = q `ISet.member` acceptingStates
dfaAccepts _ Nothing                        = False

instance Ord c => FiniteAutomaton (Dfa c) DfaState c Bool where
    faStates =
        Set.union (Set.singleton Nothing) .
        Set.map Just . Set.fromAscList . ISet.toAscList . dfaStates
    faOutput = dfaAccepts
    faInputs = dfaAlphabet
    faTransitions dfa (Just q) =
        Map.map (Set.singleton . Just) -- Wrap each state into a singleton set
         .
        Map.mapKeys snd -- Remove the state
         .
        Map.filterWithKey (const . (== q) . fst) -- Pick the transitions from state p
         .
        dfaTransitions $
        dfa
    faTransitions dfa Nothing =
        Map.fromSet (const (Set.singleton Nothing)) -- Each input leads to `Nothing`
         .
        faInputs $
        dfa

-- | This function builds a DFA and returns the initial state. This is a convenience to explicitly
-- start the DFA from the initial state, allowing it to be treated uniformly with other states.
-- The function will return Nothing if there are overlapping transitions.
buildDfa :: Ord c => [Int] -> [((Int, c), Int)] -> Maybe (Dfa c)
buildDfa finalStates transitions =
    if Map.size transitionMap < length transitions
        then Nothing
        else Just dfa
  where
    transitionMap = Map.fromList transitions
    dfa = Dfa (ISet.fromList finalStates) transitionMap

buildDfaUnsafe :: Ord c => [Int] -> [((Int, c), Int)] -> Dfa c
buildDfaUnsafe finalStates transitions =
    Dfa (ISet.fromList finalStates) $ Map.fromList transitions

-- | Adds the given number to all states of the given automata.
translateDfaStates :: (Ord c) => Dfa c -> Int -> Dfa c
translateDfaStates (Dfa acceptingStates transitions) offset =
    Dfa acceptingStates' transitions'
  where
    acceptingStates' = ISet.map (+ offset) acceptingStates
    transitions' =
        Map.map (+ offset) . Map.mapKeys (\(q, c) -> (q + offset, c)) $
        transitions

runDfa :: (Ord c, Foldable t) => Dfa c -> DfaState -> t c -> DfaState
runDfa dfa = Foldable.foldl (dfaStep dfa)

dfaStep :: (Ord c) => Dfa c -> DfaState -> c -> DfaState
dfaStep _ Nothing _ = dfaErrorState
dfaStep (Dfa _ transitions) (Just state) input =
    Map.lookup (state, input) transitions
