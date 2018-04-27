{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module:      Data.Dfa.Internal
Description: 
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)


-}
module Data.Dfa.Internal
    ( Dfa(..)
    , DfaState(..)
    , dfaAlphabet
    , dfaAccepts
    , dfaErrorState
    , dfaStates
    ) where

import           Data.FiniteAutomaton

import           Data.IntSet          ( IntSet )
import qualified Data.IntSet          as ISet
import           Data.Map             ( Map )
import qualified Data.Map             as Map
import qualified Data.Set             as Set

data Dfa c = Dfa
    { acceptingStatesSet :: IntSet
    , transitionMap      :: Map (Int, c) Int
    } deriving (Eq, Show)

newtype DfaState = DfaState
    { toStateNumber :: Maybe Int
    } deriving (Eq, Ord, Show)

dfaErrorState :: DfaState
dfaErrorState = DfaState Nothing

-- | Computes the states used by this DFA. This is the set of all accepting states and the states
-- which take part in any transition. /The error state will not be returned/
dfaStates :: Dfa c -> IntSet
dfaStates (Dfa acceptingStates transitions) = acceptingStates `ISet.union` transitionStates
  where
    transitionStates = Map.foldMapWithKey comb transitions
    comb (p, _) q = ISet.fromList [p, q]

-- | Computes the alphabet of the DFA.
-- This is the set of all characters for which any transition is defined.
dfaAlphabet :: (Ord c) => Dfa c -> Set.Set c
dfaAlphabet (Dfa _ transitions) = Set.map snd . Map.keysSet $ transitions

dfaAccepts :: Dfa c -> DfaState -> Bool
dfaAccepts (Dfa acceptingStates _) (DfaState (Just q)) = q `ISet.member` acceptingStates
dfaAccepts _ (DfaState Nothing) = False

instance Ord c => FiniteAutomaton (Dfa c) DfaState c Bool where
    faStates =
        Set.union (Set.singleton dfaErrorState) .
        Set.map (DfaState . Just) . Set.fromAscList . ISet.toAscList . dfaStates
    faOutput = dfaAccepts
    faInputs = dfaAlphabet
    faTransitions dfa (DfaState (Just q)) =
        Map.map (Set.singleton . DfaState . Just) -- Wrap each state into a singleton set
        . Map.mapKeys snd -- Remove the state
        . Map.filterWithKey (const . (== q) . fst) -- Pick the transitions from state p
        . transitionMap
        $ dfa
    faTransitions dfa (DfaState Nothing) =
        -- Each input leads to `Nothing`
        Map.fromSet (const (Set.singleton dfaErrorState)) $ faInputs dfa
