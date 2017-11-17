{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Algorithm.DfaEquivalence where

import           Algorithm.AutomataMerge
import           Data.Dfa
import           Data.Equivalence.Monad
import           Data.Queue                      as Q

import           Control.Monad                   ( when )
import           Control.Monad.Trans.Class       ( lift )
import           Control.Monad.Trans.Writer.Lazy
import           Data.Foldable
import qualified Data.IntSet                     as ISet
import           Data.Maybe                      ( isNothing )
import           Data.Sequence                   as Seq
import qualified Data.Set                        as Set

data Error =
    NotDfaState Int
    deriving (Eq, Show)

type Constraint c = ([c], DfaState, DfaState)

type HkNaiveM c = Writer (Seq (Bool, Constraint c))

dfaStatesEquivalentHkNaive ::
       (Ord c) => Dfa c -> DfaState -> DfaState -> Either Error Bool
dfaStatesEquivalentHkNaive dfa s1 s2 =
    (isNothing . fst) <$> dfaStatesDifferencesHkNaive dfa s1 s2

-- | Find an input word which disproves the equality of the given DFA states,
-- Also the states the automata are in (after reading the input word) is returned.
-- For each constraint to check a trace is produced.
-- The flag in the traces indicates whether that check was redundant, and thus was skipped.
-- This implementation just uses a set of pairs to record equalities.
dfaStatesDifferencesHkNaive ::
       forall c. (Ord c)
    => Dfa c
    -> DfaState
    -> DfaState
    -> Either Error (Maybe (Constraint c), [(Bool, Constraint c)])
dfaStatesDifferencesHkNaive dfa s1 s2 = do
    s1 `assertIsStateOf` dfa
    s2 `assertIsStateOf` dfa
    let (witness, traces) =
            runWriter $ check Set.empty (Q.singleton ([], s1, s2))
    return (witness, toList traces)
  where
    check ::
           Set.Set (DfaState, DfaState)
        -> FifoQueue (Constraint c)
        -> HkNaiveM c (Maybe (Constraint c))
    check bisim queue =
        (flip . maybe) (return Nothing) (Q.pop queue) $ \case
            (constraint@(_w, x, y), queue')
                | (x, y) `Set.member` bisim ->
                    skip constraint >> check bisim queue'
                | (dfa `dfaAccepts` x) /= (dfa `dfaAccepts` y) ->
                    return (Just constraint)
                | otherwise -> do
                    trace constraint
                    check
                        ((x, y) `Set.insert` bisim)
                        (queue' `Q.pushAll` todo constraint)
      where
        todo (w, x, y) =
            [(w ++ [c], x `dfaStep'` c, y `dfaStep'` c) | c <- alphabet]
    alphabet = Set.toList (dfaAlphabet dfa)
    dfaStep' = dfaStep dfa
    trace :: Constraint c -> HkNaiveM c () -- Necessary to help the type checker
    trace = tell . Seq.singleton . (False, )
    skip :: Constraint c -> HkNaiveM c () -- Necessary to help the type checker
    skip = tell . Seq.singleton . (True, )

dfaStatesEquivalentHk ::
       forall c. (Ord c)
    => Dfa c
    -> DfaState
    -> DfaState
    -> Either Error Bool
dfaStatesEquivalentHk dfa s1 s2 =
    (isNothing . fst) <$> dfaStatesDifferencesHk dfa s1 s2

type HkEquivM c s = EquivT' s DfaState (Writer (Seq (Bool, Constraint c)))

-- | Find an input word which disproves the equality of the given DFA states,
-- Also the states the automata are in (after reading the input word) is returned.
-- For each constraint to check a trace is produced.
-- The flag in the traces indicates whether that check was redundant, and thus was skipped.
-- This implementation uses a Union-Find data structure.
dfaStatesDifferencesHk ::
       forall c. (Ord c)
    => Dfa c
    -> DfaState
    -> DfaState
    -> Either Error (Maybe (Constraint c), [(Bool, Constraint c)])
dfaStatesDifferencesHk dfa s1 s2 = do
    s1 `assertIsStateOf` dfa
    s2 `assertIsStateOf` dfa
    let (witness, traces) =
            runWriter $ runEquivT' $ check (Q.singleton ([], s1, s2))
    return (witness, toList traces)
  where
    check :: FifoQueue (Constraint c) -> HkEquivM c s (Maybe (Constraint c))
    check queue =
        (flip . maybe) (return Nothing) (Q.pop queue) $ \case
            (constraint@(_w, x, y), queue') -> do
                alreadyEqual <- equivalent x y
                if alreadyEqual
                    then skip constraint >> check queue'
                    else if (dfa `dfaAccepts` x) /= (dfa `dfaAccepts` y)
                             then return (Just constraint)
                             else do
                                 equate x y
                                 trace constraint
                                 check (queue' `Q.pushAll` todo constraint)
      where
        todo (w, x, y) =
            [(w ++ [c], x `dfaStep'` c, y `dfaStep'` c) | c <- alphabet]
    alphabet = Set.toList (dfaAlphabet dfa)
    dfaStep' = dfaStep dfa
    trace :: Constraint c -> HkEquivM c s () -- Necessary to help the type checker
    trace = lift . tell . Seq.singleton . (False, )
    skip :: Constraint c -> HkEquivM c s () -- Necessary to help the type checker
    skip = lift . tell . Seq.singleton . (True, )

assertIsStateOf :: DfaState -> Dfa c -> Either Error ()
assertIsStateOf Nothing _ = return ()
assertIsStateOf (Just q) dfa =
    when (q `ISet.notMember` dfaStates dfa) $ Left (NotDfaState q)

dfaEquivalentHkNaive ::
       Ord c => (Int, Dfa c) -> (Int, Dfa c) -> Either Error Bool
dfaEquivalentHkNaive (p, dfa1) (q, dfa2) =
    dfaStatesEquivalentHkNaive mergedAutomata (Just p) (Just (toMergedState q))
  where
    (toMergedState, mergedAutomata) = mergeDfa dfa1 dfa2

dfaEquivalentHk :: Ord c => (Int, Dfa c) -> (Int, Dfa c) -> Either Error Bool
dfaEquivalentHk (p, dfa1) (q, dfa2) =
    dfaStatesEquivalentHk mergedAutomata (Just p) (Just (toMergedState q))
  where
    (toMergedState, mergedAutomata) = mergeDfa dfa1 dfa2
