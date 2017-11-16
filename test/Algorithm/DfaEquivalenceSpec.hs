module Algorithm.DfaEquivalenceSpec
    ( main
    , spec
    ) where

import Algorithm.DfaEquivalence
import Data.Dfa

import Control.Monad            ( forM_ )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    forM_ [ ("dfaEquivalentHkNaive", dfaEquivalentHkNaive), ("dfaEquivalentHk", dfaEquivalentHk) ] $
        \(name, dfaEquivalent) ->
            describe name $ do
                it "should tell apart the automata for {a, b} and {a}" $ do
                    let (startState, acceptingState) = (0, 1)
                        dfa1 = buildDfaUnsafe [ acceptingState ]
                                              [ ((startState, 'a'), acceptingState)
                                              , ((startState, 'b'), acceptingState)
                                              ]
                        dfa2 = buildDfaUnsafe [ acceptingState ]
                                              [ ((startState, 'a'), acceptingState) ]
                    dfaEquivalent (startState, dfa1) (startState, dfa2) `shouldBe` Right False

                it "should prove that isomorphic versions of a+ are the same" $ do
                    let (firstState, secondState) = (0, 1)
                        dfa1 = buildDfaUnsafe [ secondState ]
                                              [ ((firstState, 'a'), secondState)
                                              , ((secondState, 'a'), secondState)
                                              ]
                        dfa2 = buildDfaUnsafe [ firstState ]
                                              [ ((secondState, 'a'), firstState)
                                              , ((firstState, 'a'), firstState)
                                              ]
                    dfaEquivalent (firstState, dfa1) (secondState, dfa2) `shouldBe` Right True

                it "should prove that two simple automata with only accepting states are equal" $ do
                    let (firstState, secondState) = (0, 1)
                        dfa1 = buildDfaUnsafe [ firstState ] [ ((firstState, 'a'), firstState) ]
                        dfa2 = buildDfaUnsafe [ firstState, secondState ]
                                              [ ((firstState, 'a'), secondState)
                                              , ((secondState, 'a'), firstState)
                                              ]
                    dfaEquivalent (firstState, dfa1) (firstState, dfa2) `shouldBe` Right True

                it "should prove that the DFA expansions of two equivalent NFAs are equal" $ do
                    let dfa1 = buildDfaUnsafe [ 2, 4, 5, 6 ]
                                              [ ((1, 'a'), 2)
                                              , ((2, 'a'), 3)
                                              , ((3, 'a'), 4)
                                              , ((4, 'a'), 5)
                                              , ((5, 'a'), 6)
                                              , ((6, 'a'), 6)
                                              ]
                    let dfa2 = buildDfaUnsafe [ 2, 4 ]
                                              [ ((1, 'a'), 2)
                                              , ((2, 'a'), 3)
                                              , ((3, 'a'), 4)
                                              , ((4, 'a'), 4)
                                              ]

                    dfaEquivalent (1, dfa1) (1, dfa2) `shouldBe` Right True

                it "should prove that " $ do
                    let dfa1 = buildDfaUnsafe [ 2, 3 ]
                                              [ ((1, 'a'), 2)
                                              , ((1, 'b'), 3)
                                              , ((2, 'a'), 3)
                                              , ((2, 'b'), 3)
                                              , ((3, 'a'), 2)
                                              , ((3, 'b'), 2)
                                              ]

                        dfa2 = buildDfaUnsafe [ 2, 3 ]
                                              [ ((1, 'a'), 2)
                                              , ((1, 'b'), 2)
                                              , ((2, 'a'), 3)
                                              , ((2, 'b'), 3)
                                              , ((3, 'a'), 3)
                                              , ((3, 'b'), 3)
                                              ]

                    dfaEquivalent (1, dfa1) (1, dfa2) `shouldBe` Right True

    forM_ [ ("dfaStatesEquivalentHkNaive", dfaStatesEquivalentHkNaive)
          , ("dfaStatesEquivalentHk", dfaStatesEquivalentHk)
          ] $
        \(name, dfaStatesEquivalent) ->
            describe name $ do
                it "should tell apart accepting and error states" $
                    let dfa = buildDfaUnsafe [ 1 ] []
                    in
                        dfaStatesEquivalent dfa (Just 1) dfaErrorState `shouldBe` Right False

                it "should not tell apart the error state and a custom non-accepting catch-all state" $
                    let dfa = buildDfaUnsafe [] [ ((1, 'a'), 1) ]
                    in
                        dfaStatesEquivalent dfa (Just 1) dfaErrorState `shouldBe` Right True

                it "should tell apart a state accepting {a} and a state accepting {b}" $
                    let dfa = buildDfaUnsafe [ 2, 4 ] [ ((1, 'a'), 2), ((3, 'b'), 4) ]
                    in
                        dfaStatesEquivalent dfa (Just 1) (Just 3) `shouldBe` Right False

                it "should not tell apart two states both accepting {a}" $
                    let dfa = buildDfaUnsafe [ 2, 4 ] [ ((1, 'a'), 2), ((3, 'a'), 4) ]
                    in
                        dfaStatesEquivalent dfa (Just 1) (Just 3) `shouldBe` Right True
