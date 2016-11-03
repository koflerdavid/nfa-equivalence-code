module Data.NfaSpec (main, spec) where

import Control.Monad (forM_)
import Test.Hspec

import Data.Nfa

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "nfa for \"\"" $ do
    let nfa = buildNfa [0] []

    it "should accept \"\"" $
      ([0], nfa) `shouldAccept` ""

    it "should not accept \"a\"" $ do
      ([0], nfa) `shouldNotAccept` "a"

  describe "nfa for 'a'" $ do
    let nfa = buildNfa [1] [((0, 'a'), [1])]

    it "should accept \"a\"" $ do
      ([0], nfa) `shouldAccept` "a"

    forM_ ["", "aa", "b", "ab", "ba"] $ \input -> do
      it ("should not accept " ++ show input) $
        ([0], nfa) `shouldNotAccept` input

  describe "nfa for a?b" $ do
    let nfa = buildNfa [2] [((0, 'a'), [1]),
                            ((0, 'b'), [2]),
                            ((1, 'b'), [2])]

    forM_ ["b", "ab"] $ \input -> do
      it ("should accept " ++ show input) $ do
        ([0], nfa) `shouldAccept` input

    forM_ ["", "aa", "ba", "bb", "c", "bc", "abc"] $ \input -> do
      it ("should not accept " ++ show input) $
        ([0], nfa) `shouldNotAccept` input

shouldAccept :: (Ord c, Show c) => ([Int], Nfa c) -> [c] -> Expectation
shouldAccept (initialStates, nfa) input = runNfa nfa initialStates input `shouldSatisfy` (nfa `accepts`)

shouldNotAccept :: (Ord c, Show c) => ([Int], Nfa c) -> [c] -> Expectation
shouldNotAccept (initialStates, nfa) input = runNfa nfa initialStates input `shouldNotSatisfy` (nfa `accepts`)
