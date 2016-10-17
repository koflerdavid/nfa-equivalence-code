{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.DfaEquivalence where

import Data.Dfa
import Data.Equivalence.Monad

import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Sequence

-- The alphabets have to be the same. Although it could be the case that
-- automata's alphabets differ, while the languages are the same, the automatons
-- can still always be modified to operate on the same input alphabet.
-- Also, the algorithm was originally written to calculate the equalivalence
-- of two states of the <strong>same</strong> automata, which renders this point moot anyways.
dfaEquivalentHkNaive :: Ord c => Dfa c -> Dfa c -> Bool
dfaEquivalentHkNaive dfa1 dfa2
  | dfaAlphabet dfa1 /= dfaAlphabet dfa2 = False

  | otherwise = check S.empty (viewl $ singleton (dfaInitialState dfa1, dfaInitialState dfa2))
      where check _ EmptyL = True
            check bisim (pair@(x,y) :< ps)
              | pair `S.member` bisim = check bisim (viewl ps)
              | (x `IS.member` dfaFinalStates dfa1) /= (y `IS.member` dfaFinalStates dfa2) = False
              | otherwise = check ((x, y) `S.insert` bisim) (viewl $ fromList todo >< ps)
                  where todo = [(transitionsDfa1 M.! (x, c), transitionsDfa2 M.! (y, c)) | c <- alphabet]

            alphabet = S.toList (dfaAlphabet dfa1) -- We already know this is the same for dfa2
            transitionsDfa1 = dfaTransitionFunction dfa1
            transitionsDfa2 = dfaTransitionFunction dfa2

dfaEquivalentHk :: Ord c => Dfa c -> Dfa c -> Bool
dfaEquivalentHk dfa1 dfa2
  | dfaAlphabet dfa1 /= dfaAlphabet dfa2 = False

  | otherwise = runEquivM' $ check (viewl $ singleton initialPair)
      where initialPair = (dfaInitialState dfa1, dfaInitialState dfa2)

            check :: ViewL (Int, Int) -> EquivM' s Int Bool
            check EmptyL = return True
            check ((x,y) :< ps) = do
              alreadyEqual <- equivalent x y
              if alreadyEqual then check (viewl ps)
              else if (x `IS.member` dfaFinalStates dfa1) /= (y `IS.member` dfaFinalStates dfa2) then return False
              else equate x y >> check (viewl $ fromList todo >< ps)
                where todo = [(transitionsDfa1 M.! (x, c), transitionsDfa2 M.! (y, c)) | c <- alphabet]

            alphabet = S.toList (dfaAlphabet dfa1) -- We already know this is the same for dfa2
            transitionsDfa1 = dfaTransitionFunction dfa1
            transitionsDfa2 = dfaTransitionFunction dfa2
