{-# LANGUAGE ViewPatterns #-}

{- |
Module:      Algorithm.Regex.DfaConversion.Internal
Description: 
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)
-}
module Algorithm.Regex.DfaConversion.Internal
    ( RegexDfaTransitions
    , convertStateTransitions
    , ensureInitialStateIsPresent
    , getAndIncrement
    , markAsAcceptingStates
    , recordTransition
    ) where

import           Data.Dfa                  ( Dfa, DfaState )
import           Data.Dfa.Builder
import           Data.Regex                ( Regex (Empty) )

import           Control.Monad             ( forM_ )
import           Control.Monad.Trans.State ( State, gets, modify, state )
import qualified Data.Foldable             as Foldable
import           Data.Map                  ( Map )
import qualified Data.Map                  as Map
import           Data.Maybe                ( mapMaybe )

type RegexDfaTransitions c = Map (Regex c) (Map c (Regex c))

convertStateTransitions ::
       Ord c => RegexDfaTransitions c -> State (Int, Dfa c, Map (Regex c) (Int, DfaState)) ()
convertStateTransitions regexTransitions = do
    -- It is not necessary to encode `Empty`, as `Empty` and its transitions
    -- are represented by `dfaErrorState`
    forM_ (filter ((/= Empty) . fst) $ Map.toList regexTransitions) $
        -- It is not necessary to encode transitions to `Empty`
        -- as `Empty` is represented by `dfaErrorState`,
        -- and a DFA's default behaviour is to transition to there.
        \(origin, transitions) -> do
            forM_ (filter ((/= Empty) . snd) $ Map.toList transitions) $ \(input, destination) -> do
                recordTransition origin input destination

stateNumberOf :: Ord c => Regex c -> State (Int, a, Map (Regex c) (Int, DfaState)) Int
stateNumberOf regex = do
    maybeState <- gets $ \(_n, _, mapping) -> regex `Map.lookup` mapping
    case maybeState of
        Just (s, _) -> return s
        Nothing     -> getAndIncrement

recordTransition ::
       Ord c => Regex c -> c -> Regex c -> State (Int, Dfa c, Map (Regex c) (Int, DfaState)) ()
recordTransition origin c destination = do
    originStateNumber <- stateNumberOf origin
    -- One has to be careful to not request another number for the same state.
    destinationStateNumber <-
        if origin == destination
            then return originStateNumber
            else stateNumberOf destination

    modify $ \(n, dfa, mapping) ->
        let (dfa', originState, destinationState) =
                addTransitionUnsafe originStateNumber c destinationStateNumber dfa
            mapping' =
                Map.insert origin (originStateNumber, originState) .
                Map.insert destination (destinationStateNumber, destinationState) $
                mapping
        in (n, dfa', mapping')

markAsAcceptingStates ::
       (Ord c, Foldable t) => t (Regex c) -> State (Int, Dfa c, Map (Regex c) (Int, DfaState)) ()
markAsAcceptingStates (Foldable.toList -> states) = do
    modify $ \(n, dfa, mapping) ->
        let existingDfaStates = mapMaybe (fmap snd . (`Map.lookup` mapping)) states
            newRegexes = filter (not . (`Map.member` mapping)) states
            amountOfNewRegexes = length newRegexes
            newStateNumbers = [n .. (n + amountOfNewRegexes - 1)]
            (newStates, dfa') =
                withAcceptingStatesUnsafe newStateNumbers . withAcceptingStates existingDfaStates $
                dfa
            newPairs = zip newRegexes (zip newStateNumbers newStates)
            mapping' = foldr (uncurry Map.insert) mapping newPairs
        in (n + amountOfNewRegexes, dfa', mapping')

ensureInitialStateIsPresent ::
       Ord c => Regex c -> State (Int, Dfa c, Map (Regex c) (Int, DfaState)) ()
ensureInitialStateIsPresent initialRegex =
    modify $ \original@(n, dfa, mapping) ->
        if initialRegex `Map.member` mapping
            then original
            else (n + 1, dfa, Map.insert initialRegex (n, allocateState dfa n) mapping)

getAndIncrement :: State (Int, a, b) Int
getAndIncrement = state $ \(n, dfa, mapping) -> n `seq` (n, (n + 1, dfa, mapping))
