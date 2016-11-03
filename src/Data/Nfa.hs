{-# LANGUAGE ScopedTypeVariables #-}

module Data.Nfa
  (
    Nfa(..)
    , nfaAlphabet
    , nfaStates
    , buildNfa
    , runNfa
    , accepts
  ) where

import Control.Monad
import Control.Monad.Trans.RWS.Strict
import Data.IntSet as ISet
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set

import Data.Nfa.Internal

data Nfa c =
  Nfa { nfaAcceptingStates :: ISet.IntSet
      , nfaTransitions :: Map.Map (Int, Maybe c) ISet.IntSet
      }
      deriving (Eq, Show)

-- | Build an NFA out of the given transitions.
-- The main advantage over the constructor is that the input can be made using lists.
buildNfa :: (Ord c) => [Int] -> [((Int, Maybe c), [Int])] -> Nfa c
buildNfa acceptingStates
    = Nfa (ISet.fromList acceptingStates)
    . Map.fromListWith ISet.union
    . List.map (mapSnd ISet.fromList) -- Use sets to represent target states

nfaAlphabet :: (Ord c) => Nfa c -> Set.Set c
nfaAlphabet
    = Set.mapMonotonic fromJust -- This is valid because forall x, y: Just x < Just y <=> x < y
    . Set.filter isJust -- We are interested in all non-epsilon transitions.
    . Set.map snd -- Only extract the characters. This operation is not monotonic!
    . Map.keysSet
    . nfaTransitions

nfaStates :: Nfa c -> ISet.IntSet
nfaStates nfa = nfaAcceptingStates nfa `ISet.union` transitionStates nfa
    where
        transitionStates
            = ISet.unions
            . List.map (\ ((q, _), qs) -> q `ISet.insert` qs) -- Collect the states
            . Map.toAscList
            . nfaTransitions

runNfa :: (Ord c, Show c) => Nfa c -> [Int] -> [c] -> [Int]
runNfa nfa initialStates input =
  let table = nfaTransitions nfa
      initialStatesSet = ISet.fromList initialStates
      clInitialStates = closure table initialStatesSet
      finalStates = fst $ execRWS (forM_ input nfaStep) table clInitialStates
  in ISet.toList $ finalStates

accepts :: Nfa c -> [Int] -> Bool
accepts nfa possibleStates = not . ISet.null $ nfaAcceptingStates nfa `ISet.intersection` ISet.fromList possibleStates

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
