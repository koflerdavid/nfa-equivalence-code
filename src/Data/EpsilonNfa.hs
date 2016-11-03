module Data.EpsilonNfa
  (
    ENfa(..)
    , enfaAlphabet
    , enfaStates
    , buildEnfa
    , runEnfa
    , accepts
  ) where

import Control.Monad
import Control.Monad.Trans.RWS.Strict
import Data.IntSet as ISet
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set

import Data.EpsilonNfa.Internal

data ENfa c =
  ENfa { enfaAcceptingStates :: ISet.IntSet
       , enfaTransitions :: Map.Map (Int, Maybe c) ISet.IntSet
       }
       deriving (Eq, Show)

-- | Build an NFA out of the given transitions.
-- The main advantage over the constructor is that the input can be made using lists.
buildEnfa :: (Ord c) => [Int] -> [((Int, Maybe c), [Int])] -> ENfa c
buildEnfa acceptingStates
    = ENfa (ISet.fromList acceptingStates)
    . Map.fromListWith ISet.union
    . List.map (mapSnd ISet.fromList) -- Use sets to represent target states

enfaAlphabet :: (Ord c) => ENfa c -> Set.Set c
enfaAlphabet
    = Set.mapMonotonic fromJust -- This is valid because forall x, y: Just x < Just y <=> x < y
    . Set.filter isJust -- We are interested in all non-epsilon transitions.
    . Set.map snd -- Only extract the characters. This operation is not monotonic!
    . Map.keysSet
    . enfaTransitions

enfaStates :: ENfa c -> ISet.IntSet
enfaStates enfa = enfaAcceptingStates enfa `ISet.union` transitionStates enfa
    where
        transitionStates
            = ISet.unions
            . List.map (\ ((q, _), qs) -> q `ISet.insert` qs) -- Collect the states
            . Map.toAscList
            . enfaTransitions

runEnfa :: (Ord c, Show c) => ENfa c -> [Int] -> [c] -> [Int]
runEnfa enfa initialStates input =
  let table = enfaTransitions enfa
      initialStatesSet = ISet.fromList initialStates
      clInitialStates = closure table initialStatesSet
      finalStates = fst $ execRWS (forM_ input enfaStep) table clInitialStates
  in ISet.toList $ finalStates

accepts :: ENfa c -> [Int] -> Bool
accepts enfa possibleStates = not . ISet.null $ enfaAcceptingStates enfa `ISet.intersection` ISet.fromList possibleStates

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
