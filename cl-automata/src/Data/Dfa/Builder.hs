{- |
Module:      Data.Dfa.Builder
Description: An API to construct DFAs.
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)

It all starts with `emptyDfa` or any other existing DFA.
From there, new state transitions can be added, or states can be marked as accepting.
-}

module Data.Dfa.Builder
  ( addTransition
  , addTransitionUnsafe
  , emptyDfa
  , withAcceptingStates
  , withAcceptingStatesUnsafe
  ) where

import Data.Dfa
import Data.Dfa.Internal

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.IntSet as ISet

emptyDfa :: Ord c => Dfa c
emptyDfa = buildDfaUnsafe [] []

-- | Adds a transition to a DFA. A check is performed to ensure that the new transition
-- is not overlapping. If this is the case, a `Left` value with the input and the destination
-- state of the existing transition is returned.
-- Else, a `Right` value with the modified DFA and the converted origin and destination states is returned.
addTransition :: Ord c => Int -> c -> Int -> Dfa c -> Either DfaState (Dfa c, DfaState, DfaState)
addTransition from input to dfa =
    case transitionMap dfa Map.!? (from, input) of
        Just existingDestinationState
            | existingDestinationState == to -> Right (dfa, DfaState (Just from), DfaState (Just to))
            | otherwise -> Left (DfaState (Just existingDestinationState))
        Nothing -> Right (addTransitionUnsafe from input to dfa)

addTransitionUnsafe :: Ord c => Int -> c -> Int -> Dfa c -> (Dfa c, DfaState, DfaState)
addTransitionUnsafe from input to dfa =
    let extendedDfa = Dfa {
                          acceptingStatesSet = acceptingStatesSet dfa
                        , transitionMap = Map.insert (from, input) to (transitionMap dfa)
                       }
    in (extendedDfa, DfaState (Just from), DfaState (Just to))

withAcceptingStates :: [DfaState] -> Dfa c -> Dfa c
withAcceptingStates stateList =
    snd . withAcceptingStatesUnsafe (catMaybes . map toStateNumber $ stateList)

withAcceptingStatesUnsafe :: [Int] -> Dfa c -> ([DfaState], Dfa c)
withAcceptingStatesUnsafe stateList dfa =
    let newStateSet = ISet.fromList stateList
        dfa' = Dfa {
                    acceptingStatesSet = newStateSet `ISet.union` acceptingStatesSet dfa
                  , transitionMap = transitionMap dfa
                }
    in (map (DfaState . Just) stateList, dfa')
