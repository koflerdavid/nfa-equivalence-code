module Algorithm.DfaEquivalenceSpec
    ( spec_dfaEquivalence
    ) where

import Algorithm.DfaEquivalence
import Data.Dfa

import Control.Monad            ( forM_ )
import Test.Hspec

spec_dfaEquivalence :: Spec
spec_dfaEquivalence = do
    forM_ [("dfaEquivalentHkNaive", dfaEquivalentHkNaive), ("dfaEquivalentHk", dfaEquivalentHk)] $ \(name, dfaEquivalent) ->
        describe name $ do
            it "should tell apart the automata for {a, b} and {a}" $ do
                let (startState, acceptingState) = (0, 1)
                    dfa1 =
                        buildDfaUnsafe
                            [acceptingState]
                            [ ((startState, 'a'), acceptingState)
                            , ((startState, 'b'), acceptingState)
                            ]
                    dfa2 = buildDfaUnsafe [acceptingState] [((startState, 'a'), acceptingState)]
                    Just startState'  = toDfaState dfa1 startState
                    Just startState'' = toDfaState dfa2 startState
                dfaEquivalent (startState', dfa1) (startState'', dfa2) `shouldBe` False

            it "should prove that isomorphic versions of a+ are the same" $ do
                let (firstState, secondState) = (0, 1)
                    dfa1 =
                        buildDfaUnsafe
                            [secondState]
                            [ ((firstState, 'a'), secondState)
                            , ((secondState, 'a'), secondState)
                            ]
                    dfa2 =
                        buildDfaUnsafe
                            [firstState]
                            [ ((secondState, 'a'), firstState)
                            , ((firstState, 'a'), firstState)
                            ]
                    Just firstState'  = toDfaState dfa1 firstState
                    Just secondState' = toDfaState dfa2 secondState
                dfaEquivalent (firstState', dfa1) (secondState', dfa2) `shouldBe` True

            it "should prove that two simple automata with only accepting states are equal" $ do
                let (firstState, secondState) = (0, 1)
                    dfa1 = buildDfaUnsafe [firstState] [((firstState, 'a'), firstState)]
                    dfa2 =
                        buildDfaUnsafe
                            [firstState, secondState]
                            [((firstState, 'a'), secondState), ((secondState, 'a'), firstState)]
                    Just firstState' = toDfaState dfa1 firstState
                dfaEquivalent (firstState', dfa1) (firstState', dfa2) `shouldBe` True

            it "should prove that the DFA expansions of two equivalent NFAs are equal" $ do
                let dfa1 =
                        buildDfaUnsafe
                            [2, 4, 5, 6]
                            [ ((1, 'a'), 2)
                            , ((2, 'a'), 3)
                            , ((3, 'a'), 4)
                            , ((4, 'a'), 5)
                            , ((5, 'a'), 6)
                            , ((6, 'a'), 6)
                            ]
                let dfa2 =
                        buildDfaUnsafe
                            [2, 4]
                            [((1, 'a'), 2), ((2, 'a'), 3), ((3, 'a'), 4), ((4, 'a'), 4)]
                    Just state1' = toDfaState dfa1 1
                    Just state1'' = toDfaState dfa2 1
                dfaEquivalent (state1', dfa1) (state1'', dfa2) `shouldBe` True

            it "should prove that " $ do
                let dfa1 =
                        buildDfaUnsafe
                            [2, 3]
                            [ ((1, 'a'), 2)
                            , ((1, 'b'), 3)
                            , ((2, 'a'), 3)
                            , ((2, 'b'), 3)
                            , ((3, 'a'), 2)
                            , ((3, 'b'), 2)
                            ]
                    dfa2 =
                        buildDfaUnsafe
                            [2, 3]
                            [ ((1, 'a'), 2)
                            , ((1, 'b'), 2)
                            , ((2, 'a'), 3)
                            , ((2, 'b'), 3)
                            , ((3, 'a'), 3)
                            , ((3, 'b'), 3)
                            ]
                    Just state1' = toDfaState dfa1 1
                    Just state1'' = toDfaState dfa2 1
                dfaEquivalent (state1', dfa1) (state1'', dfa2) `shouldBe` True

    forM_
        [ ("dfaStatesEquivalentHkNaive", dfaStatesEquivalentHkNaive)
        , ("dfaStatesEquivalentHk", dfaStatesEquivalentHk)
        ] $ \(name, dfaStatesEquivalent) ->
        describe name $ do
            it "should tell apart accepting and error states" $
                let dfa = buildDfaUnsafe [1] []
                    Just state1 = toDfaState dfa 1
                in dfaStatesEquivalent dfa state1 dfaErrorState `shouldBe` False

            it "should not tell apart the error state and a custom non-accepting catch-all state" $
                let dfa = buildDfaUnsafe [] [((1, 'a'), 1)]
                    Just state1 = toDfaState dfa 1
                in dfaStatesEquivalent dfa state1 dfaErrorState `shouldBe` True

            it "should tell apart a state accepting {a} and a state accepting {b}" $
                let dfa = buildDfaUnsafe [2, 4] [((1, 'a'), 2), ((3, 'b'), 4)]
                    Just state1 = toDfaState dfa 1
                    Just state3 = toDfaState dfa 3
                in dfaStatesEquivalent dfa state1 state3 `shouldBe` False

            it "should not tell apart two states both accepting {a}" $
                let dfa = buildDfaUnsafe [2, 4] [((1, 'a'), 2), ((3, 'a'), 4)]
                    Just state1 = toDfaState dfa 1
                    Just state3 = toDfaState dfa 3
                in dfaStatesEquivalent dfa state1 state3 `shouldBe` True
