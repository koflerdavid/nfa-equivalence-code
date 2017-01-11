{-# LANGUAGE LambdaCase #-}

module Algorithm.DfaEquivalence where

import           Algorithm.AutomataMerge
import           Data.Dfa
import           Data.Equivalence.Monad
import           Data.Queue              as Q

import           Control.Monad           ( when )
import qualified Data.IntSet             as IS
import qualified Data.Set                as S

data Error = NotDfaState Int
    deriving (Eq, Show)

dfaStatesEquivalentHkNaive :: Ord c => Dfa c -> DfaState -> DfaState -> Either Error Bool
dfaStatesEquivalentHkNaive dfa s1 s2 = do
    s1 `assertIsStateOf` dfa
    s2 `assertIsStateOf` dfa
    return $ check S.empty (Q.singleton (s1, s2))
  where
    check :: S.Set (DfaState, DfaState) -> FifoQueue (DfaState, DfaState) -> Bool
    check bisim queue = (flip . maybe) True (Q.pop queue) $
        \case
            (pair@(x, y), queue')
                | pair `S.member` bisim ->
                      check bisim queue'
                | (dfa `dfaAccepts` x) /= (dfa `dfaAccepts` y) ->
                      False
                | otherwise -> check ((x, y) `S.insert` bisim) (queue' `Q.pushAll` todo x y)
      where
        todo x y = [ (x `dfaStep'` c, y `dfaStep'` c)
                   | c <- alphabet ]

    alphabet = S.toList (dfaAlphabet dfa)
    --            dfaStep' :: (Ord c) => Dfa c -> (DfaState, c) -> DfaState
    dfaStep' = dfaStep dfa

dfaStatesEquivalentHk :: Ord c => Dfa c -> DfaState -> DfaState -> Either Error Bool
dfaStatesEquivalentHk dfa s1 s2 = do
    s1 `assertIsStateOf` dfa
    s2 `assertIsStateOf` dfa
    return (runEquivM' $ check (Q.singleton (s1, s2)))
  where
    check :: FifoQueue (DfaState, DfaState) -> EquivM' s DfaState Bool
    check queue = (flip . maybe) (return True) (Q.pop queue) $
        \case
            ((x, y), queue') -> do
                alreadyEqual <- equivalent x y
                if alreadyEqual
                    then check queue'
                    else if (dfa `dfaAccepts` x) /= (dfa `dfaAccepts` y)
                         then return False
                         else equate x y >> check (queue' `Q.pushAll` todo x y)
      where
        todo x y = [ (x `dfaStep'` c, y `dfaStep'` c)
                   | c <- alphabet ]

    alphabet = S.toList (dfaAlphabet dfa)
    dfaStep' = dfaStep dfa

assertIsStateOf :: DfaState -> Dfa c -> Either Error ()
assertIsStateOf Nothing _ =
    return ()
assertIsStateOf (Just q) dfa =
    when (q `IS.notMember` dfaStates dfa) $ Left (NotDfaState q)

dfaEquivalentHkNaive :: Ord c => (Int, Dfa c) -> (Int, Dfa c) -> Either Error Bool
dfaEquivalentHkNaive (p, dfa1) (q, dfa2) =
    dfaStatesEquivalentHkNaive mergedAutomata (Just p) (Just (toMergedState q))
  where
    (toMergedState, mergedAutomata) =
        mergeDfa dfa1 dfa2

dfaEquivalentHk :: Ord c => (Int, Dfa c) -> (Int, Dfa c) -> Either Error Bool
dfaEquivalentHk (p, dfa1) (q, dfa2) =
    dfaStatesEquivalentHk mergedAutomata (Just p) (Just (toMergedState q))
  where
    (toMergedState, mergedAutomata) =
        mergeDfa dfa1 dfa2
