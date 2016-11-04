module Data.DfaSpec
    ( main
    , spec
    ) where

import           Control.Monad ( forM_ )
import           Data.IntSet   as IS
import           Data.Set      as Set

import           Test.Hspec

import           Data.Dfa

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "runDfa" $ do
        it "accepts \"ab\" and \"abab\"" $ do
            forM_ [ "ab", "abab" ] $
                \input -> do
                    (dfa `dfaAccepts` runDfa dfa (Just startState) input) `shouldBe` True

        it "does not accept the empty string, \"aba\", \"abb\" and \"aabbaa\"" $ do
            forM_ [ "", "aba", "abb", "aabbaa" ] $
                \input -> do
                    (dfa `dfaAccepts` runDfa dfa (Just startState) input) `shouldBe` False

    describe "buildDfa" $ do
        it "dfaStates should contain the accepting state(s) of the DFA" $ do
            let builtDfa = buildDfaUnsafe [ 1 ] [ ((0, 'a'), 1) ]
            dfaAcceptingStates builtDfa `shouldSatisfy` (`IS.isSubsetOf` dfaStates builtDfa)

    describe "dfaAlphabet" $ do
        it "the alphabet of the sample DFA should be {'a','b'}" $ do
            dfaAlphabet dfa `shouldBe` Set.fromList [ 'a', 'b' ]

startState :: Int
startState = 0

acceptingState :: Int
acceptingState = 2

dfa :: Dfa Char
dfa = buildDfaUnsafe [ acceptingState ]
                     [ ((startState, 'a'), 1)
                     , ((1, 'b'), acceptingState)
                     , ((acceptingState, 'a'), 1)
                     ]
