{-# LANGUAGE ViewPatterns #-}

module Algorithm.AutomataMergeSpec
    ( spec_automataMerge
    , prop_mergingShouldPreserveDfa1
    , prop_mergingShouldPreserveDfa2
    , prop_mergedDfaStatesDontOverlap
    , prop_mergingWithEmptyAutomatonIsLeftIdempotent
    , prop_mergingWithEmptyAutomatonIsRightIdempotent
    ) where

import Algorithm.AutomataMerge ( mergeDfa )
import Data.Dfa
import Data.Dfa.Builder        ( emptyDfa )
import Data.Dfa.Test           ( NonEmptyDfa (..) )
import Data.FiniteAutomaton    ( faStates )

import Control.Monad           ( forM_ )

import Data.Maybe              ( fromJust )
import Data.Set                as Set
import Test.Hspec
import Test.Invariant          ( idempotent )
import Test.QuickCheck

spec_automataMerge :: Spec
spec_automataMerge = do
    describe "union" $ do
        forM_ samples $ \(i, (dfa1, dfa2, merged)) -> do
            it ("should work for sample #" ++ show i) $ do
                snd (dfa1 `mergeDfa` dfa2) `shouldBe` merged

samples :: [(Int, (Dfa Char, Dfa Char, Dfa Char))]
samples = [1 ..] `zip` [sample1]

sample1 :: (Dfa Char, Dfa Char, Dfa Char)
sample1 =
    ( fromJust $ buildDfa [1] [((0, 'a'), 1)]
    , fromJust $ buildDfa [0] [((0, 'a'), 0), ((0, 'b'), 1)]
    , fromJust $ buildDfa [1, 2] [((0, 'a'), 1), ((2, 'a'), 2), ((2, 'b'), 3)])

prop_mergingShouldPreserveDfa1 :: NonEmptyDfa Char -> Dfa Char -> Property
prop_mergingShouldPreserveDfa1 (toDfa -> dfa1) dfa2 =
    let (_liftState, mergedAutomaton) = mergeDfa dfa1 dfa2
        alphabet = dfaAlphabet dfa1
        transitionsDfa1 = [(s, c) | s <- Set.toList (faStates dfa1), c <- Set.toList alphabet]
    in forAll (elements transitionsDfa1) $ \(s, c) ->
           dfaStep dfa1 s c === dfaStep mergedAutomaton s c

prop_mergingShouldPreserveDfa2 :: Dfa Char -> NonEmptyDfa Char -> Property
prop_mergingShouldPreserveDfa2 dfa1 (toDfa -> dfa2) =
    let (liftState, mergedAutomaton) = mergeDfa dfa1 dfa2
        alphabet = dfaAlphabet dfa2
        transitionsDfa2 = [(s, c) | s <- Set.toList (faStates dfa2), c <- Set.toList alphabet]
    in forAll (elements transitionsDfa2) $ \(s, c) ->
           liftState (dfaStep dfa2 s c) === dfaStep mergedAutomaton (liftState s) c

prop_mergedDfaStatesDontOverlap :: Dfa Char -> Dfa Char -> Property
prop_mergedDfaStatesDontOverlap dfa1 dfa2 =
    let (liftState, mergedAutomaton) = mergeDfa dfa1 dfa2
    in (faStates mergedAutomaton Set.\\ faStates dfa1) ===
       (Set.map liftState (faStates dfa2) \\ Set.singleton dfaErrorState)

prop_mergingWithEmptyAutomatonIsLeftIdempotent :: Dfa Char -> Bool
prop_mergingWithEmptyAutomatonIsLeftIdempotent = idempotent $ snd . (`mergeDfa` emptyDfa)

prop_mergingWithEmptyAutomatonIsRightIdempotent :: Dfa Char -> Bool
prop_mergingWithEmptyAutomatonIsRightIdempotent = idempotent $ snd . (emptyDfa `mergeDfa`)
