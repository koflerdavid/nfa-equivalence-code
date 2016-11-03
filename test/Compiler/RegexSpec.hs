module Compiler.RegexSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Compiler.Regex
import Data.Nfa
import Data.Regex

import Control.Monad (forM_)
import Data.Set (fromList)

main :: IO ()
main = do
  let regexNfa = compileRegex (Atom 'a')
  putStrLn (show regexNfa)
  hspec spec

spec = do
  describe "run a" $ do
    let nfa = compileRegex (Atom 'a')
    it "accepts \"a\"" $ do
      nfa `accepts` runNfa nfa [0] "a" `shouldBe` True
    it "does not accept \"\", \"b\", \"aa\", \"ab\"" $ do
      forM_ ["", "b", "aa", "ab"] $ \input -> do
        nfa `accepts` runNfa nfa [0] input `shouldBe` False

  describe "run (a|b)*" $ do
    let nfa = compileRegex (Asterisk (Alternative [Atom 'a', Atom 'b']))
    it "accepts the empty string, \"a\", \"b\" and \"ab\"" $ do
      forM_ ["", "a", "b", "ab"] $ \input -> do
        (nfa `accepts` runNfa nfa[0] input) `shouldBe` True

    it "does not accept \"d\"" $ do
      (nfa `accepts` runNfa nfa [0] "d") `shouldBe` False

  describe "run a*" $ do
    let nfa = compileRegex (Asterisk (Atom 'a'))
    it "accepts \"\", \"a\", \"aa\", \"aaa\"" $ do
      forM_ ["", "a", "aa", "aaa"] $ \input ->
        nfa `accepts` runNfa nfa [0] input `shouldBe` True
    it "does not accept \"b\"" $ do
      nfa `accepts` runNfa nfa [0] "b" `shouldBe` False
