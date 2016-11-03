module Data.EpsilonNfaSpec (main, spec) where

import Control.Monad (forM_)
import Test.Hspec

import Data.EpsilonNfa

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "enfa for \"\"" $ do
    let nfa = buildEnfa [0] []

    it "should accept \"\"" $
      ([0], nfa) `shouldAccept` ""

    it "should not accept \"a\"" $ do
      ([0], nfa) `shouldNotAccept` "a"

  describe "enfa for 'a'" $ do
    let nfa = buildEnfa [1] [((0, Just 'a'), [1])]

    it "should accept \"a\"" $ do
      ([0], nfa) `shouldAccept` "a"

    forM_ ["", "aa", "b", "ab", "ba"] $ \input -> do
      it ("should not accept " ++ show input) $
        ([0], nfa) `shouldNotAccept` input

  describe "enfa for a?b" $ do
    let nfa = buildEnfa [3] [((0, Just 'a'), [1]),
                            ((0, Nothing), [2]),
                            ((1, Just 'b'), [3]),
                            ((2, Just 'b'), [3])]

    forM_ ["b", "ab"] $ \input -> do
      it ("should accept " ++ show input) $ do
        ([0], nfa) `shouldAccept` input

    forM_ ["", "aa", "ba", "bb", "c", "bc", "abc"] $ \input -> do
      it ("should not accept " ++ show input) $
        ([0], nfa) `shouldNotAccept` input

shouldAccept :: (Ord c, Show c) => ([Int], ENfa c) -> [c] -> Expectation
shouldAccept (initialStates, enfa) input = runEnfa enfa initialStates input `shouldSatisfy` (enfa `accepts`)

shouldNotAccept :: (Ord c, Show c) => ([Int], ENfa c) -> [c] -> Expectation
shouldNotAccept (initialStates, enfa) input = runEnfa enfa initialStates input `shouldNotSatisfy` (enfa `accepts`)
