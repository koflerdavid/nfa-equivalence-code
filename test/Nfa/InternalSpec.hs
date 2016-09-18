module Nfa.InternalSpec (main, spec) where

import Control.Monad.Trans.RWS.Strict
import Data.Nfa.Internal
import Data.IntSet

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "nfaStep'ping" $ do
    let (firstState, secondState, thirdState, fourthState) = (1, 2, 3, 4)
        transitions = transitionTable [((firstState, Just 'a'), [secondState]),
                                       ((firstState, Nothing), [thirdState]),
                                       ((thirdState, Just 'c'), [fourthState])]

    it "should be possible to do an 'a' step" $ do
      execRWS (nfaStep 'a') transitions (singleton firstState) `shouldBe` (singleton secondState, ())

    it "should not be possible to do a 'b' step" $ do
      execRWS (nfaStep 'b') transitions (singleton firstState) `shouldBe` (empty, ())

    it "should be possible to do a 'c' step (via the third state)" $ do
      execRWS (nfaStep 'c') transitions (singleton firstState) `shouldBe` (singleton fourthState, ())

  describe "step" $ do
    let (firstState, secondState, thirdState) = (0, 1, 2)
        transitions = transitionTable [((firstState, Just 'a'), [secondState]),
                                       ((firstState, Nothing), [thirdState])]

    it "should go from firstState with 'a' to secondState" $ do
      step transitions (singleton firstState) (Just 'a') `shouldBe` singleton secondState

  describe "closure" $ do
    let (firstState, secondState, thirdState) = (1, 2, 3)
        transitions = transitionTable [((firstState, Just 'a'), [secondState]),
                                       ((firstState, Nothing), [thirdState])]

    it "should compute the closure of the first state correctly" $ do
      transitions `closure` singleton firstState `shouldBe` fromList [firstState, thirdState]

    it "should compute the closure of the second state correctly" $ do
      transitions `closure` singleton secondState `shouldBe` fromList [secondState]

    it "should compute the closure of the third state correctly" $ do
      transitions `closure` singleton thirdState `shouldBe` fromList [thirdState]

  describe "epsilonReachable" $ do
    let (firstState, secondState, thirdState) = (1, 2, 3)
        transitions = transitionTable [((firstState, Just 'a'), [secondState]),
                                       ((firstState, Nothing), [thirdState])]

    it "should be the third state from the first state" $ do
      transitions `getEpsilonReachableStates` firstState `shouldBe` singleton thirdState

    it "should be none from the second and the third state" $ do
      transitions `getEpsilonReachableStates` secondState `shouldBe` empty
      transitions `getEpsilonReachableStates` thirdState `shouldBe` empty
