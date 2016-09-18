module Nfa.InternalSpec (main, spec) where

import Control.Monad.Trans.RWS.Strict
import Data.Nfa.Internal
import Data.Set

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "nfaStep'ping" $ do
    let (initialState, nextState, otherState) = (0, 1, 2)
        transitions = transitionTable [((initialState, Just 'a'), [nextState]),
                                       ((initialState, Nothing), [otherState])]

    it "should be possible to do an 'a' step" $ do
      execRWS (nfaStep 'a') transitions (singleton initialState) `shouldBe` (singleton nextState, ())

    it "should be possible to do an epsilon-step" $ do
      step transitions (singleton initialState) Nothing `shouldBe` fromList [initialState, otherState]

    it "should not be possible to do a 'b' step" $ do
      execRWS (nfaStep 'b') transitions (singleton initialState) `shouldBe` (empty, ())

  describe "step" $ do
    let (firstState, secondState, thirdState) = (0, 1, 2)
        transitions = transitionTable [((firstState, Just 'a'), [secondState]),
                                       ((firstState, Nothing), [thirdState])]

    it "should step from 0 with 'a' to 1" $ do
      step transitions (singleton firstState) (Just 'a') `shouldBe` singleton thirdState

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
