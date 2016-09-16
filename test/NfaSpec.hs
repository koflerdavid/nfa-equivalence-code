module NfaSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Nfa
import Data.Regex
import Algorithm.RegexCompiler

import Control.Monad (forM_)
import Data.Set (fromList)

main :: IO ()
main = hspec spec

nfa :: Nfa Int Char
nfa = compileRegex (Asterisk (Alternative [Atom 'a', Atom 'b']))

spec :: Spec
spec = do
  describe "runNfa" $ do
    it "accepts the empty string, \"a\", \"b\" and \"ab\"" $ do
      forM_ ["", "a", "b", "ab"] $ \input -> do
        (nfa `accepts` runNfa nfa input) `shouldBe` True

    it "does not accept \"d\"" $ do
      (nfa `accepts` runNfa nfa "d") `shouldBe` False
