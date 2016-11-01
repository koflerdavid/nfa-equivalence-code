module Data.DfaSpec (main, spec) where


import Control.Monad (forM_)
import Data.IntSet as IS
import Data.Set as Set

import Test.Hspec
import Test.QuickCheck

import Data.Dfa


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runDfa" $ do
    it "accepts \"ab\" and \"abab\"" $ do
      forM_ ["ab", "abab"] $ \input -> do
        dfa `accepts` runDfa dfa input `shouldBe` True

    it "does not accept the empty string, \"aba\", \"abb\" and \"aabbaa\"" $ do
      forM_ ["", "aba", "abb", "aabbaa"] $ \input -> do
        dfa `accepts` runDfa dfa input `shouldBe` False

  describe "buildDfa" $ do
    it "dfaStates should contain the initial, the final and the error state(s) of the DFA" $ do
      let dfa = buildDfa 0 [1] [((0, 'a'), 1)]
      dfaFinalStates dfa `shouldSatisfy` (`IS.isSubsetOf` dfaStates dfa)
      dfaInitialState dfa `shouldSatisfy` (`IS.member` dfaStates dfa)
      dfaErrorState dfa `shouldSatisfy` (`IS.member` dfaStates dfa)

  describe "dfaAlphabet" $ do
    it "the alphabet of the sample DFA should be {'a','b'}" $ do
      dfaAlphabet dfa `shouldBe` Set.fromList ['a', 'b']

startState = 0
acceptingState = 2

dfa :: Dfa Char
dfa = buildDfa startState [acceptingState] [((0, 'a'), 1), ((1, 'b'), 2), ((2, 'a'), 1)]
