{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module:      Data.Dfa.Test
Description: Contains utility code for testing DFA's
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)
-}
module Data.Dfa.Test
    ( NonEmptyDfa(..)
    , hasAtLeastNStates
    , prop_nonEmptyDfaIsNotEmpty
    , sameAcceptingStatesSet
    , sameTransitionMap
    , someDfaInput
    , someDfaState
    ) where

import           Data.Dfa
import           Data.Dfa.Internal

import           Control.Monad     ( forM )
import qualified Data.IntSet       as ISet
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import           Test.QuickCheck

instance Arbitrary DfaState where
    arbitrary = DfaState <$> (Just <$> arbitrary)

instance (Arbitrary c, Ord c) => Arbitrary (Dfa c) where
    arbitrary = do
        states <- listOf (arbitrary :: Gen Int)
        alphabet <- listOf (arbitrary :: Gen c)
        generateDfa states alphabet

newtype NonEmptyDfa c = MkNonEmptyDfa
    { toDfa :: Dfa c
    } deriving (Eq, Show)

instance (Arbitrary c, Ord c) => Arbitrary (NonEmptyDfa c) where
    arbitrary = do
        states <- listOf1 (arbitrary :: Gen Int)
        alphabet <- listOf1 (arbitrary :: Gen c)
        MkNonEmptyDfa <$> generateDfa states alphabet

generateDfa :: Ord c => [Int] -> [c] -> Gen (Dfa c)
generateDfa states alphabet = do
    acceptingStates <- sublistOf states
    transitionList <-
        forM [(from, c) | from <- states, c <- alphabet] $ \key -> do
            state <- elements states
            return $! (key, state)
    return $
        Dfa
        { acceptingStatesSet = ISet.fromList acceptingStates
        , transitionMap = Map.fromList transitionList
        }

prop_nonEmptyDfaIsNotEmpty :: NonEmptyDfa Char -> Property
prop_nonEmptyDfaIsNotEmpty (toDfa -> dfa) =
    (not . ISet.null) (dfaStates dfa) .&&. (not . Set.null) (dfaAlphabet dfa)

hasAtLeastNStates :: Int -> Dfa c -> Bool
hasAtLeastNStates n dfa = ISet.size (dfaStates dfa) >= n

sameAcceptingStatesSet :: Dfa c -> Dfa c -> Property
sameAcceptingStatesSet (acceptingStatesSet -> set1) (acceptingStatesSet -> set2) = set1 === set2

sameTransitionMap :: (Ord c, Show c) => Dfa c -> Dfa c -> Property
sameTransitionMap (transitionMap -> ts1) (transitionMap -> ts2) = ts1 === ts2

someDfaInput :: Ord c => Dfa c -> Gen c
someDfaInput dfa = elements $ Set.toList (dfaAlphabet dfa)

someDfaState :: Dfa c -> Gen (Int, DfaState)
someDfaState dfa = do
    state <- elements $ ISet.toList (dfaStates dfa)
    return (state, DfaState (Just state))
