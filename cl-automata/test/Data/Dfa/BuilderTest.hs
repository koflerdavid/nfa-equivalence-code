{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module:      Data.Dfa.BuilderTest
Description: Checks the abilities of the DFA builder API.
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)
-}
module Data.Dfa.BuilderTest where

import           Data.Dfa

import           Data.Dfa.Builder
import           Data.Dfa.Test

import           Data.Either       ( isRight )

import qualified Data.IntSet       as ISet
import qualified Data.Map          as Map
import           Test.QuickCheck

prop_emptyDfaHasNoValidStates :: DfaState -> Char -> Bool
prop_emptyDfaHasNoValidStates state input =
    let dfa = emptyDfa
    in not $ dfaAccepts dfa (dfaStep dfa state input)

prop_emptyDfaHasNoTransitions :: Property
prop_emptyDfaHasNoTransitions = dfaTransitions (emptyDfa :: Dfa Char) === Map.empty

prop_newTransitionIsRecognized :: Int -> Char -> Int -> Property
prop_newTransitionIsRecognized origin input destination =
    let originalDfa = emptyDfa
        Right (dfa, originState, destinationState) =
            addTransition origin input destination originalDfa
    in dfaStep dfa originState input == destinationState .&&. sameAcceptingStatesSet originalDfa dfa

prop_overlappingTransitionForbidden :: NonEmptyDfa Char -> Property
prop_overlappingTransitionForbidden (toDfa -> dfa) =
    -- Else, there is no alternative state one could transition to
    (hasAtLeastNStates 2 dfa ==>) $ do
        (origin, originState) <- someDfaState dfa
        input <- someDfaInput dfa
        let existingDestinationState = dfaStep dfa originState input
            Just existingDestination = toStateNumber existingDestinationState
            possibleDestinations = filter (/= existingDestination) theStates
        destination <- elements possibleDestinations
        let result = addTransition origin input destination dfa
        return $ result === Left existingDestinationState
  where
    theStates = ISet.toList (dfaStates dfa)

prop_addingTransitionIsIdempotent :: NonEmptyDfa Char -> Gen Property
prop_addingTransitionIsIdempotent (toDfa -> dfa) = do
    (origin, originState) <- someDfaState dfa
    input <- someDfaInput dfa
    let existingDestinationState = dfaStep dfa originState input
        Just existingDestination = toStateNumber existingDestinationState
        result@(Right (newDfa, start, end)) = addTransition origin input existingDestination dfa
    return $ isRight result
        .&&. dfa === newDfa
        .&&. originState == start
        .&&. existingDestinationState == end

prop_markingTransitionsMakesThemAccepting :: DfaState -> Dfa Char -> Property
prop_markingTransitionsMakesThemAccepting state dfa =
    not (dfa `dfaAccepts` state) ==>
    let  extendedDfa = withAcceptingStates [state] dfa
    in extendedDfa `dfaAccepts` state
           .&&. sameTransitionMap dfa extendedDfa
