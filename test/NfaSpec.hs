module NfaSpec (main, spec) where

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
    it "should accept \"\"" $ do
      nfa `accepts` runNfa nfa "" `shouldBe` True
    it "should not accept \"a\"" $ do
      nfa `accepts` runNfa nfa "a" `shouldBe` False

  describe "nfa for 'a'" $ do
    let nfa = Nfa ['a'] [0, 1] [0] [1] [((0, Just 'a'), [1])]
    it "accepts \"a\"" $ do
      nfa `accepts` runNfa nfa "a" `shouldBe` True
