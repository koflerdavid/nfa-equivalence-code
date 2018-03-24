{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.EpsilonNfa
    ( ENfa(..)
    , enfaAlphabet
    , enfaStates
    , buildEnfa
    , runEnfa
    , accepts
    ) where

import           Data.Bifunctor                 ( second )
import           Control.Monad                  ( forM_ )
import           Control.Monad.Trans.RWS.Strict ( execRWS )
import           Data.IntSet                    as ISet
import           Data.List                      as List
import           Data.Map                       as Map
import           Data.Maybe                     ( fromJust, isJust )
import qualified Data.Set                       as Set

import           Data.EpsilonNfa.Internal
import           Data.FiniteAutomaton

data ENfa c = ENfa
    { enfaAcceptingStates :: ISet.IntSet
    , enfaTransitions     :: Map.Map (Int, Maybe c) ISet.IntSet
    } deriving (Eq, Show)

-- | Build an NFA out of the given transitions.
-- The main advantage over the constructor is that the input can be made using lists.
buildEnfa :: (Ord c) => [Int] -> [((Int, Maybe c), [Int])] -> ENfa c
buildEnfa acceptingStates =
    ENfa (ISet.fromList acceptingStates) .
    Map.fromListWith ISet.union .
    List.map (second ISet.fromList) -- Use sets to represent target states

enfaAlphabet :: (Ord c) => ENfa c -> Set.Set c
enfaAlphabet =
    Set.mapMonotonic fromJust -- Valid because forall x, y: Just x < Just y <=> x < y
    . Set.filter isJust -- We are interested in all non-epsilon transitions.
    . Set.map snd -- Only extract the characters. This operation is not monotonic!
    . Map.keysSet
    . enfaTransitions

enfaStates :: ENfa c -> ISet.IntSet
enfaStates enfa = enfaAcceptingStates enfa `ISet.union` transitionStates enfa
  where
    transitionStates =
        ISet.unions
        . List.map (\((q, _), qs) -> q `ISet.insert` qs) -- Collect the states
        . Map.toAscList
        . enfaTransitions

runEnfa :: (Ord c) => ENfa c -> [Int] -> [c] -> [Int]
runEnfa enfa initialStates input = ISet.toList finalStates
  where
    table = enfaTransitions enfa
    initialStatesSet = ISet.fromList initialStates
    clInitialStates = closure table initialStatesSet
    finalStates = fst $ execRWS (forM_ input enfaStep) table clInitialStates

accepts :: ENfa c -> [Int] -> Bool
accepts enfa possibleStates =
    not . ISet.null $
        enfaAcceptingStates enfa `ISet.intersection` ISet.fromList possibleStates

instance Ord c => FiniteAutomaton (ENfa c) Int (Maybe c) Bool where
    faStates = Set.fromDistinctAscList . ISet.toAscList . enfaStates
    faInputs = Set.union (Set.singleton Nothing) . Set.map Just . enfaAlphabet
    faOutput enfa = (`ISet.member` enfaAcceptingStates enfa)
    faTransitions enfa q =
        Map.map (Set.fromDistinctAscList . ISet.toAscList) -- Convert IntSets to Sets
        . Map.mapKeys snd -- Remove the state
        . Map.filterWithKey (const . (== q) . fst) -- Select only the relevant transitions
        . enfaTransitions
        $ enfa
