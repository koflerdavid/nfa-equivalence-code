module NfaSpec (main, spec) where

import Control.Monad (forM_)
import Test.Hspec
import Test.QuickCheck

import Data.Nfa
import Data.Regex
import Algorithm.RegexCompiler

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "nfa for \"\"" $ do
    let nfa = Nfa ['a'] [0] [0] [0] []

    it "should accept \"\"" $
      nfa `shouldAccept` ""

    it "should not accept \"a\"" $ do
      nfa `shouldNotAccept` "a"

  describe "nfa for 'a'" $ do
    let nfa = Nfa ['a'] [0, 1] [0] [1] [((0, Just 'a'), [1])]

    it "should accept \"a\"" $ do
      nfa `shouldAccept` "a"

    forM_ ["", "aa", "b", "ab", "ba"] $ \input -> do
      it ("should not accept " ++ show input) $
        nfa `shouldNotAccept` input

  describe "nfa for a?b" $ do
    let nfa = Nfa ['a', 'b'] [0, 1, 2, 3] [0] [3] [((0, Just 'a'), [1]),
                                                   ((0, Nothing), [2]),
                                                   ((1, Just 'b'), [3]),
                                                   ((2, Just 'b'), [3])]

    forM_ ["b", "ab"] $ \input -> do
      it ("should accept " ++ show input) $ do
        nfa `shouldAccept` input

    forM_ ["", "aa", "ba", "bb", "c", "bc", "abc"] $ \input -> do
      it ("should not accept " ++ show input) $
        nfa `shouldNotAccept` input

shouldAccept :: (Ord c, Show c) => Nfa c -> [c] -> Expectation
shouldAccept nfa input = runNfa nfa input `shouldSatisfy` (nfa `accepts`)

shouldNotAccept :: (Ord c, Show c) => Nfa c -> [c] -> Expectation
shouldNotAccept nfa input = runNfa nfa input `shouldNotSatisfy` (nfa `accepts`)
