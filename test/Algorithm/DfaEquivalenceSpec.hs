module Algorithm.DfaEquivalenceSpec (main, spec) where

import Control.Monad (forM_)
import Test.Hspec
import Test.QuickCheck

import Algorithm.DfaEquivalence
import Data.Dfa

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  forM_ [("dfaEquivalentHkNaive", dfaEquivalentHkNaive), ("dfaEquivalentHk", dfaEquivalentHk)] $ \(name, dfaEquivalent) ->
    describe name $ do

      it "should tell apart the automata for {a, b} and {a}" $ do
        let (startState, acceptingState) = (0, 1)
            dfa1 = buildDfa startState [acceptingState] [((startState, 'a'), acceptingState), ((startState, 'b'), acceptingState)]
            dfa2 = buildDfa startState [acceptingState] [((startState, 'a'), acceptingState)]
        dfaEquivalent dfa1 dfa2 `shouldBe` False

      it "should prove that isomorphic versions of a+ are the same" $ do
        let (firstState, secondState) = (0, 1)
            dfa1 = buildDfa firstState [secondState] [((firstState, 'a'), secondState), ((secondState, 'a'), secondState)]
            dfa2 = buildDfa secondState [firstState] [((secondState, 'a'), firstState), ((firstState, 'a'), firstState)]
        dfaEquivalent dfa1 dfa2 `shouldBe` True

      it "should prove that two simple automata with only accepting states are equal" $ do
        let (firstState, secondState) = (0, 1)
            dfa1 = buildDfa firstState [firstState] [((firstState, 'a'), firstState)]
            dfa2 = buildDfa firstState [firstState, secondState] [((firstState, 'a'), secondState),
                                                                  ((secondState, 'a'), firstState)]
        dfaEquivalent dfa1 dfa2 `shouldBe` True

      it "should prove that the DFA expansions of two equivalent NFAs are equal" $ do
        let dfa1 = buildDfa 1 [2, 4, 5, 6] [((1, 'a'), 2), ((2, 'a'), 3),
                                            ((3, 'a'), 4), ((4, 'a'), 5),
                                            ((5, 'a'), 6), ((6, 'a'), 6)]
        let dfa2 = buildDfa 1 [2, 4] [((1, 'a'), 2), ((2, 'a'), 3),
                                      ((3, 'a'), 4), ((4, 'a'), 4)]

        dfaEquivalent dfa1 dfa2 `shouldBe` True

      it "should prove that " $ do
        let dfa1 = buildDfa 1 [2, 3] [((1, 'a'), 2), ((1, 'b'), 3),
                                      ((2, 'a'), 3), ((2, 'b'), 3),
                                      ((3, 'a'), 2), ((3, 'b'), 2)]

            dfa2 = buildDfa 1 [2, 3] [((1, 'a'), 2), ((1, 'b'), 2),
                                      ((2, 'a'), 3), ((2, 'b'), 3),
                                      ((3, 'a'), 3), ((3, 'b'), 3)]

        dfaEquivalent dfa1 dfa2 `shouldBe` True
