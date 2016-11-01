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

dfaStatesEquivalentHkNaive :: Ord c => Dfa c -> Int -> Int -> Either Error Bool
dfaStatesEquivalentHkNaive dfa s1 s2 = do
    s1 `assertIsStateOf` dfa
    s2 `assertIsStateOf` dfa
    return $ check S.empty (viewl $ singleton (s1, s2))
        where
            check _ EmptyL = True
            check bisim (pair@(x,y) :< ps)
                | pair `S.member` bisim = check bisim (viewl ps)
                | (x `IS.member` finalStates) /= (y `IS.member` finalStates) = False
                | otherwise = check ((x, y) `S.insert` bisim) (viewl $ fromList todo >< ps)
                    where
                        todo = [(dfa `dfaStep` (x, c), dfa `dfaStep` (y, c)) | c <- alphabet]

            alphabet = S.toList (dfaAlphabet dfa)
            finalStates = dfaFinalStates dfa


dfaStatesEquivalentHk :: Ord c => Dfa c -> Int -> Int -> Either Error Bool
dfaStatesEquivalentHk dfa s1 s2 = do
    s1 `assertIsStateOf` dfa
    s2 `assertIsStateOf` dfa
    return (runEquivM' $ check (viewl $ singleton (s1, s2)))
        where
            check :: ViewL (Int, Int) -> EquivM' s Int Bool
            check EmptyL = return True
            check ((x,y) :< ps) = do
                alreadyEqual <- equivalent x y
                if alreadyEqual then check (viewl ps)
                else if (x `IS.member` finalStates) /= (y `IS.member` finalStates) then return False
                else equate x y >> check (viewl $ fromList todo >< ps)
                    where todo = [(dfa `dfaStep` (x, c), dfa `dfaStep` (y, c)) | c <- alphabet]

            alphabet = S.toList (dfaAlphabet dfa)
            finalStates = dfaFinalStates dfa


assertIsStateOf :: Int -> Dfa c -> Either Error ()
assertIsStateOf s dfa = when (s `IS.notMember` dfaStates dfa) $ Left (NotDfaState s)


dfaEquivalentHkNaive :: Ord c => Dfa c -> Dfa c -> Either Error Bool
dfaEquivalentHkNaive dfa1 dfa2 =
    let (initialStateDfa2', mergedAutomata) = mergeDfa dfa1 dfa2
    in dfaStatesEquivalentHkNaive mergedAutomata (dfaInitialState dfa1) initialStateDfa2'


dfaEquivalentHk :: Ord c => Dfa c -> Dfa c -> Either Error Bool
dfaEquivalentHk dfa1 dfa2 =
    let (initialStateDfa2', mergedAutomata) = mergeDfa dfa1 dfa2
    in dfaStatesEquivalentHk mergedAutomata (dfaInitialState dfa1) initialStateDfa2'
