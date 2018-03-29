module Compiler.RegexSpec
    ( main
    , spec
    ) where

import Control.Monad   ( forM_ )
import Test.Hspec

import Compiler.Regex
import Data.EpsilonNfa
import Data.Regex

main :: IO ()
main = do
    let regexENfa = compileRegex (Atom 'a')
    print regexENfa
    hspec spec

spec :: Spec
spec = do
    describe "run a" $ do
        let enfa = compileRegex (Atom 'a')
        it "accepts \"a\"" $ do
            (enfa `accepts` runEnfa enfa [0] "a") `shouldBe` True
        it "does not accept \"\", \"b\", \"aa\", \"ab\"" $ do
            forM_ ["", "b", "aa", "ab"] $ \input -> do
                (enfa `accepts` runEnfa enfa [0] input) `shouldBe` False
    describe "run (a|b)*" $ do
        let enfa = compileRegex (KleeneStar (Alternative (Atom 'a') (Atom 'b')))
        it "accepts the empty string, \"a\", \"b\" and \"ab\"" $ do
            forM_ ["", "a", "b", "ab"] $ \input -> do
                (enfa `accepts` runEnfa enfa [0] input) `shouldBe` True
        it "does not accept \"d\"" $ do
            (enfa `accepts` runEnfa enfa [0] "d") `shouldBe` False
    describe "run a*" $ do
        let enfa = compileRegex (KleeneStar (Atom 'a'))
        it "accepts \"\", \"a\", \"aa\", \"aaa\"" $ do
            forM_ ["", "a", "aa", "aaa"] $ \input ->
                enfa `accepts` runEnfa enfa [0] input `shouldBe` True
        it "does not accept \"b\"" $ do
            (enfa `accepts` runEnfa enfa [0] "b") `shouldBe` False
