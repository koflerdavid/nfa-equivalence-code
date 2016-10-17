{-# LANGUAGE ScopedTypeVariables #-}

module Data.Nfa
  (
    Nfa(..)
    , runNfa
    , accepts
  ) where

import Control.Monad
import Control.Monad.Trans.RWS.Strict
import qualified Data.IntSet as IS

import Data.Nfa.Internal

data Nfa c =
  Nfa { nfaAlphabet :: [c]
      , nfaStates :: [Int]
      , nfaInitialStates :: [Int]
      , nfaFinalStates :: [Int]
      , nfaTransitionFunction :: [((Int, Maybe c), [Int])]
      }
      deriving (Eq, Show)

runNfa :: (Ord c, Show c) => Nfa c -> [c] -> [Int]
runNfa nfa input =
  let table = transitionTable (nfaTransitionFunction nfa)
      initialStates = IS.fromList (nfaInitialStates nfa)
      clInitialStates = closure table initialStates
      finalStates = fst $ execRWS (forM_ input nfaStep) table clInitialStates
  in IS.toList $ finalStates

accepts :: Nfa c -> [Int] -> Bool
accepts nfa possibleStates = not . IS.null $ IS.fromList (nfaFinalStates nfa) `IS.intersection` IS.fromList possibleStates
