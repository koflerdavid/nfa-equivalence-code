module Algorithm.NfaEquivalenceSpec
    ( prop_acceptingNfaStateSetsAreNotEqualToTheEmptySet
    , prop_eachNfaStateIsEqualToItself
    , spec_nfaEquivalence
    ) where

import           Data.Nfa.Test

import           Algorithm.NfaEquivalence
import           Data.FiniteAutomaton
import           Data.Nfa
import           HkntSamples

import           Control.Monad            ( forM_ )
import           Data.IntSet              ( singleton )
import qualified Data.IntSet              as ISet
import qualified Data.Set              as Set
import           Test.Hspec
import           Test.QuickCheck

spec_nfaEquivalence :: Spec
spec_nfaEquivalence =
    forM_
        [ ("HK naive", nfaStatesEquivalentHk)
        , ("HK with congruence", nfaStatesEquivalentHkC)
        ] $ \(name, nfaEquivalent) ->
        describe name $ do
            it "should prove the first example in the HKNT paper as equal" $ do
                let result =
                        nfaEquivalent
                            introductionNfaMerged
                            (singleton 3)
                            (singleton 0)
                result `shouldBe` True
            it "should prove figure 3 in the HKNT paper as equal" $ do
                let result =
                        nfaEquivalent
                            figure3NfaMerged
                            (singleton 0)
                            (singleton 1)
                result `shouldBe` True

prop_eachNfaStateIsEqualToItself :: Nfa Char -> Property
prop_eachNfaStateIsEqualToItself nfa =
    checkForVariant "nfaStatesEquivalentHk" (nfaStatesEquivalentHk nfa) .&&.
    checkForVariant "nfaStatesEquivalentHkC" (nfaStatesEquivalentHkC nfa)
  where
    checkForVariant name equivalent =
        counterexample name $
        forAll (subsetOf (faStates nfa)) $ checkStateSet equivalent . ISet.fromList

    checkStateSet equivalent qs = counterexample (show qs) $ equivalent qs qs

prop_acceptingNfaStateSetsAreNotEqualToTheEmptySet :: Nfa Char -> Property
prop_acceptingNfaStateSetsAreNotEqualToTheEmptySet nfa =
    not (Set.null (faAcceptingStates nfa)) ==>
        checkForVariant "nfaStatesEquivalentHk" (nfaStatesEquivalentHk nfa) .&&.
        checkForVariant "nfaStatesEquivalentHkC" (nfaStatesEquivalentHkC nfa)
  where
    checkForVariant name equivalent =
        counterexample name $
            forAll (acceptingStateOf nfa) $ \acceptingState ->
                forAll (subsetOf (faStates nfa)) $ \states ->
                    checkStateSet (not . uncurry equivalent) $
                        ISet.insert acceptingState (ISet.fromList states)

    checkStateSet notEquivalent qs = counterexample (show qs) $ notEquivalent (qs, ISet.empty)

    acceptingStateOf = elements . Set.toList . faAcceptingStates
