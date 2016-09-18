module DfaSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Dfa

import Control.Monad (forM_)

main :: IO ()
main = hspec spec

startState = 0
acceptingState = 2

dfa :: Dfa Char
dfa = buildDfa startState [acceptingState] [((0, 'a'), 1), ((1, 'b'), 2), ((2, 'a'), 1)]

spec :: Spec
spec = do
  describe "runDfa" $ do
    it "accepts \"ab\" and \"abab\"" $ do
      forM_ ["ab", "abab"] $ \input -> do
        runDfa dfa input `shouldBe` Right 2

    it "does not accept the empty string, \"aba\", \"abb\" and \"aabbaa\"" $ do
      forM_ ["", "aba", "abb", "aabbaa"] $ \input -> do
        runDfa dfa input `shouldNotBe` Right 2
