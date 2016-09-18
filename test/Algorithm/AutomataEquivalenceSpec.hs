module Algorithm.AutomataEquivalenceSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Algorithm.AutomataEquivalence
import Data.Dfa

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dfaEquivalentHkNaive" $ do

    it "should tell apart the automata for {a, b} and {a}" $ do
      let (startState, acceptingState) = (0, 1)
          dfa1 = buildDfa startState [acceptingState] [((startState, 'a'), acceptingState), ((startState, 'b'), acceptingState)]
          dfa2 = buildDfa startState [acceptingState] [((startState, 'a'), acceptingState)]
      dfaEquivalentHkNaive dfa1 dfa2 `shouldBe` False

    it "should prove that isomorphic versions of a+ are the same" $ do
      let (firstState, secondState) = (0, 1)
          dfa1 = buildDfa firstState [secondState] [((firstState, 'a'), secondState), ((secondState, 'a'), secondState)]
          dfa2 = buildDfa secondState [firstState] [((secondState, 'a'), firstState), ((firstState, 'a'), firstState)]
      dfaEquivalentHkNaive dfa1 dfa2 `shouldBe` True

    it "should prove that two simple automata with only accepting states are equal" $ do
      let (firstState, secondState) = (0, 1)
          dfa1 = buildDfa firstState [firstState] [((firstState, 'a'), firstState)]
          dfa2 = buildDfa firstState [firstState, secondState] [((firstState, 'a'), secondState),
                                                                ((secondState, 'a'), firstState)]
      dfaEquivalentHkNaive dfa1 dfa2 `shouldBe` True
