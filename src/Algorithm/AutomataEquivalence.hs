module Algorithm.AutomataEquivalence where

import Data.Dfa

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Data.Sequence (Seq, ViewL(..), (><))
import qualified Data.Sequence as Seq

dfaEquivalentHkNaive :: (Ord q, Ord c) => Dfa q c -> Dfa q c -> Bool
dfaEquivalentHkNaive dfa1 dfa2
  | dfaAlphabet dfa1 /= dfaAlphabet dfa2 = False
  | otherwise = check S.empty (Seq.viewl $ Seq.singleton (dfaInitialState dfa1, dfaInitialState dfa2))
      where check _ EmptyL = True
            check bisim (pair@(x,y) :< ps)
              | pair `S.member` bisim = check bisim (Seq.viewl ps)
              | (x `elem` dfaFinalStates dfa1) /= (y `elem` dfaFinalStates dfa2) = False
              | otherwise = let todo = [(transitionsDfa1 M.! (x, c), transitionsDfa2 M.! (y, c)) | c <- alphabet]
                            in check ((x, y) `S.insert` bisim) (Seq.viewl $ Seq.fromList todo >< ps)

            alphabet = dfaAlphabet dfa1 -- We already know this is the same for dfa2
            transitionsDfa1 = M.fromList (dfaTransitionFunction dfa1)
            transitionsDfa2 = M.fromList (dfaTransitionFunction dfa2)
