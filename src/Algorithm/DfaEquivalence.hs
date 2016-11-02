module Algorithm.DfaEquivalence where

import Algorithm.AutomataMerge
import Data.Dfa
import Data.Equivalence.Monad

import Control.Monad (when)
import qualified Data.IntSet as IS
import Data.Sequence
import qualified Data.Set as S

data Error = NotDfaState Int
    deriving (Eq, Show)

dfaStatesEquivalentHkNaive :: Ord c => Dfa c -> DfaState -> DfaState -> Either Error Bool
dfaStatesEquivalentHkNaive dfa s1 s2 = do
    s1 `assertIsStateOf` dfa
    s2 `assertIsStateOf` dfa
    return $ check S.empty (viewl $ singleton (s1, s2))
        where
            check :: S.Set (DfaState, DfaState) -> ViewL (DfaState, DfaState) -> Bool
            check _ EmptyL = True
            check bisim (pair@(x,y) :< ps)
                | pair `S.member` bisim = check bisim (viewl ps)
                | (dfa `dfaAccepts`  x) /= (dfa `dfaAccepts` y) = False
                | otherwise = check ((x, y) `S.insert` bisim) (viewl $ fromList todo >< ps)
                    where
                        todo = [(x `dfaStep'` c, y `dfaStep'` c) | c <- alphabet]

            alphabet = S.toList (dfaAlphabet dfa)
--            dfaStep' :: (Ord c) => Dfa c -> (DfaState, c) -> DfaState
            dfaStep' = dfaStep dfa


dfaStatesEquivalentHk :: Ord c => Dfa c -> DfaState -> DfaState -> Either Error Bool
dfaStatesEquivalentHk dfa s1 s2 = do
    s1 `assertIsStateOf` dfa
    s2 `assertIsStateOf` dfa
    return (runEquivM' $ check (viewl $ singleton (s1, s2)))
        where
            check :: ViewL (DfaState, DfaState) -> EquivM' s DfaState Bool
            check EmptyL = return True
            check ((x,y) :< ps) = do
                alreadyEqual <- equivalent x y
                if alreadyEqual then check (viewl ps)
                else if (dfa `dfaAccepts` x) /= (dfa `dfaAccepts` y) then return False
                else equate x y >> check (viewl $ fromList todo >< ps)
                    where
                        todo :: [(DfaState, DfaState)]
                        todo = [(x `dfaStep'` c, y `dfaStep'` c) | c <- alphabet]

            alphabet = S.toList (dfaAlphabet dfa)
            dfaStep' = dfaStep dfa


assertIsStateOf :: DfaState -> Dfa c -> Either Error ()
assertIsStateOf Nothing _ = return ()
assertIsStateOf (Just q) dfa = when (q `IS.notMember` dfaStates dfa) $ Left (NotDfaState q)


dfaEquivalentHkNaive :: Ord c => (Int, Dfa c) -> (Int, Dfa c) -> Either Error Bool
dfaEquivalentHkNaive (p, dfa1) (q, dfa2) =
    let (toMergedState, mergedAutomata) = mergeDfa dfa1 dfa2
    in dfaStatesEquivalentHkNaive mergedAutomata (Just p) (Just (toMergedState q))


dfaEquivalentHk :: Ord c => (Int, Dfa c) -> (Int, Dfa c) -> Either Error Bool
dfaEquivalentHk (p, dfa1) (q, dfa2) =
    let (toMergedState, mergedAutomata) = mergeDfa dfa1 dfa2
    in dfaStatesEquivalentHk mergedAutomata (Just p) (Just (toMergedState q))
