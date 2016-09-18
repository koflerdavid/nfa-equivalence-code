{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.AutomataEquivalence where

import Data.Dfa
import qualified Data.EquivalenceRelation as Eqr

import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Sequence (Seq, ViewL(..), (><))
import qualified Data.Sequence as Seq

-- The alphabets have to be the same. Although it could be the case that
-- automata's alphabets differ, while the languages are the same, the automatons
-- can still always be modified to operate on the same input alphabet.
-- Also, the algorithm was originally written to calculate the equalivalence
-- of two states of the <strong>same</strong> automata, which renders this point moot anyways.
dfaEquivalentHkNaive :: Ord c => Dfa c -> Dfa c -> Bool
dfaEquivalentHkNaive dfa1 dfa2
  | dfaAlphabet dfa1 /= dfaAlphabet dfa2 = False

  | otherwise = check S.empty (Seq.viewl $ Seq.singleton (dfaInitialState dfa1, dfaInitialState dfa2))
      where check _ EmptyL = True
            check bisim (pair@(x,y) :< ps)
              | pair `S.member` bisim = check bisim (Seq.viewl ps)
              | (x `IS.member` dfaFinalStates dfa1) /= (y `IS.member` dfaFinalStates dfa2) = False
              | otherwise = check ((x, y) `S.insert` bisim) (Seq.viewl $ Seq.fromList todo >< ps)
                  where todo = [(transitionsDfa1 M.! (x, c), transitionsDfa2 M.! (y, c)) | c <- alphabet]

            alphabet = S.toList (dfaAlphabet dfa1) -- We already know this is the same for dfa2
            transitionsDfa1 = dfaTransitionFunction dfa1
            transitionsDfa2 = dfaTransitionFunction dfa2

dfaEquivalentHk :: Ord c => Dfa c -> Dfa c -> Bool
dfaEquivalentHk dfa1 dfa2
  | dfaAlphabet dfa1 /= dfaAlphabet dfa2 = False

  | otherwise = check Eqr.empty (Seq.viewl $ Seq.singleton (dfaInitialState dfa1, dfaInitialState dfa2))
      where check _ EmptyL = True
            check bisim (pair@(x,y) :< ps)
              | bisim `Eqr.contains` pair = check bisim (Seq.viewl ps)
              | (x `IS.member` dfaFinalStates dfa1) /= (y `IS.member` dfaFinalStates dfa2) = False
              | otherwise = check ((x, y) `Eqr.insert` bisim) (Seq.viewl $ Seq.fromList todo >< ps)
                  where todo = [(transitionsDfa1 M.! (x, c), transitionsDfa2 M.! (y, c)) | c <- alphabet]

            alphabet = S.toList (dfaAlphabet dfa1) -- We already know this is the same for dfa2
            transitionsDfa1 = dfaTransitionFunction dfa1
            transitionsDfa2 = dfaTransitionFunction dfa2
