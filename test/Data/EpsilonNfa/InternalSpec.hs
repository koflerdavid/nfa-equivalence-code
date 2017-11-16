module Data.EpsilonNfa.InternalSpec
    ( main
    , spec
    ) where

import Data.EpsilonNfa.Internal

import Control.Monad.Trans.RWS.Strict
import Data.IntSet                    as ISet
import Data.Map                       as Map
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "enfaStep'ping" $ do
        let (firstState, secondState, thirdState, fourthState) =
                (1, 2, 3, 4)
            transitions = Map.fromList [ ((firstState, Just 'a'), ISet.singleton secondState)
                                       , ((firstState, Nothing), ISet.singleton thirdState)
                                       , ((thirdState, Just 'c'), ISet.singleton fourthState)
                                       ]

        it "should be possible to do an 'a' step" $ do
            execRWS (enfaStep 'a') transitions (ISet.singleton firstState)
                `shouldBe` (ISet.singleton secondState, ())

        it "should not be possible to do a 'b' step" $ do
            execRWS (enfaStep 'b') transitions (ISet.singleton firstState)
                `shouldBe` (ISet.empty, ())

        it "should be possible to do a 'c' step (via the third state)" $ do
            execRWS (enfaStep 'c') transitions (ISet.singleton firstState)
                `shouldBe` (ISet.singleton fourthState, ())

    describe "step" $ do
        let (firstState, secondState, thirdState) =
                (0, 1, 2)
            transitions = Map.fromList [ ((firstState, Just 'a'), ISet.singleton secondState)
                                       , ((firstState, Nothing), ISet.singleton thirdState)
                                       ]

        it "should go from firstState with 'a' to secondState" $ do
            step transitions (ISet.singleton firstState) (Just 'a')
                `shouldBe` ISet.singleton secondState

    describe "closure" $ do
        let (firstState, secondState, thirdState) =
                (1, 2, 3)
            transitions = Map.fromList [ ((firstState, Just 'a'), ISet.singleton secondState)
                                       , ((firstState, Nothing), ISet.singleton thirdState)
                                       ]

        it "should compute the closure of the first state correctly" $ do
            transitions `closure` ISet.singleton firstState
                `shouldBe` ISet.fromList [ firstState, thirdState ]

        it "should compute the closure of the second state correctly" $ do
            transitions `closure` ISet.singleton secondState
                `shouldBe` ISet.singleton secondState

        it "should compute the closure of the third state correctly" $ do
            transitions `closure` ISet.singleton thirdState
                `shouldBe` ISet.singleton thirdState

    describe "epsilonReachable" $ do
        let (firstState, secondState, thirdState) =
                (1, 2, 3)
            transitions = Map.fromList [ ((firstState, Just 'a'), ISet.singleton secondState)
                                       , ((firstState, Nothing), ISet.singleton thirdState)
                                       ]

        it "should be the third state from the first state" $ do
            transitions `getEpsilonReachableStates` firstState
                `shouldBe` ISet.singleton thirdState

        it "should be none from the second and the third state" $ do
            transitions `getEpsilonReachableStates` secondState
                `shouldBe` ISet.empty
            transitions `getEpsilonReachableStates` thirdState
                `shouldBe` ISet.empty
