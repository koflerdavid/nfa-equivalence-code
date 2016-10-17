module Language.Automata.HkntParserSpec (main, spec) where


import HkntSamples
import Language.Automata.HkntParser

import Control.Monad (forM_)
import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "automataParser" $ do
    it "should parse an automata with only accepting states and no transitions" $ do
      parseHknt "accept: a b" `shouldBe` Right (Result  [] ["a", "b"] [])

    it "should parse a simple automata with no accepting states" $ do
      parseHknt "a -a-> b" `shouldBe` Right (Result [("a", 'a', "b")] [] [])

    it "should parse the example from the introduction of the HKNT paper" $ do
      let expectedTransitions = [
                                  ("x", 'a', "y")
                                , ("y", 'a', "z")
                                , ("z", 'a', "x"), ("z", 'a', "y")
                                , ("u", 'a', "w"), ("u", 'a', "v")
                                , ("v", 'a', "w")
                                , ("w", 'a', "u")
                                ]
          expectedAcceptingStates = ["y", "v"]
          expectedChecks = [(["x"], Equivalence, ["u"])]
      parseHknt introductionInHkntFormat `shouldBe` Right (Result expectedTransitions expectedAcceptingStates expectedChecks)
