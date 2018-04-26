module Data.Dfa
    ( Dfa
    , DfaState()
    , toStateNumber
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
    , toDfaState
    ) where

import           Data.Dfa.Internal

import qualified Data.Foldable     as Foldable
import qualified Data.IntSet       as ISet
import qualified Data.Map          as Map

toDfaState :: Dfa c -> Int -> Maybe DfaState
toDfaState dfa q
    | q `ISet.member` dfaStates dfa = Just (DfaState (Just q))
    | otherwise = Nothing

dfaAcceptingStates :: Dfa c -> ISet.IntSet
dfaAcceptingStates (Dfa acceptingStates _) = acceptingStates

dfaTransitions :: Dfa c -> Map.Map (Int, c) Int
dfaTransitions (Dfa _ transitions) = transitions

-- | This function builds a DFA.
-- The function will return `Nothing` if there are overlapping transitions.
buildDfa :: Ord c => [Int] -> [((Int, c), Int)] -> Maybe (Dfa c)
buildDfa finalStates transitionList =
    if Map.size transitions < length transitionList
        then Nothing
        else Just dfa
  where
    transitions = Map.fromList transitionList
    dfa = Dfa {acceptingStatesSet = ISet.fromList finalStates, transitionMap = transitions}

buildDfaUnsafe :: Ord c => [Int] -> [((Int, c), Int)] -> Dfa c
buildDfaUnsafe finalStates transitions = Dfa (ISet.fromList finalStates) $ Map.fromList transitions

-- | Adds the given number to all states of the given automata.
translateDfaStates :: (Ord c) => Dfa c -> Int -> Dfa c
translateDfaStates (Dfa acceptingStates transitions) offset = Dfa acceptingStates' transitions'
  where
    acceptingStates' = ISet.map (+ offset) acceptingStates
    transitions' =
        Map.map (+ offset)
        . Map.mapKeys (\(q, c) -> (q + offset, c))
        $ transitions

runDfa :: (Ord c, Foldable t) => Dfa c -> DfaState -> t c -> DfaState
runDfa dfa = Foldable.foldl (dfaStep dfa)

dfaStep :: (Ord c) => Dfa c -> DfaState -> c -> DfaState
dfaStep _ (DfaState Nothing) _ = dfaErrorState
dfaStep (Dfa _ transitions) (DfaState (Just state)) input =
    DfaState $ Map.lookup (state, input) transitions
