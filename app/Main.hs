{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Dfa
import Data.Nfa
import Data.Regex

import Options.Generic

import Algorithm.RegexCompiler
import Algorithm.DfaEquivalence

data Action = DfaNfaTest | EquivalenceTest
            deriving (Generic, Show)

instance ParseRecord Action

main :: IO ()
main = do
  action <- getRecord "Automata algorithms test"

  case action of
    DfaNfaTest -> do
      let dfa = buildDfa 0 [2] [((0,'a'), 1), ((1, 'b'), 2), ((2, 'a'), 1)] :: Dfa Char
      putStrLn $ show (runDfa dfa "ab")
      putStrLn $ show (runDfa dfa "aba")
      putStrLn $ show (runDfa dfa "abb")
      putStrLn $ show (runDfa dfa "abab")
      putStrLn $ show (runDfa dfa "aabbaa")
      putStrLn $ show (runDfa dfa "")

      let nfa = compileRegex (Asterisk (Alternative [Atom 'a', Atom 'b']))
      putStrLn $ show nfa
      putStrLn $ show (runNfa nfa "a")
      putStrLn $ show (runNfa nfa "b")
      putStrLn $ show (runNfa nfa "d")
      putStrLn $ show (runNfa nfa "ab")
      putStrLn $ show (runNfa nfa "")
    EquivalenceTest -> do
      let dfa1 = buildDfa 0 [1, 2] [((0, 'a'), 1), ((0, 'b'), 1), ((1, 'a'), 2), ((1, 'b'), 2), ((2, 'a'), 2), ((2, 'b'), 2)] :: Dfa Char
          dfa2 = buildDfa 0 [1, 2] [((0, 'a'), 1), ((0, 'b'), 2), ((1, 'a'), 2), ((1, 'b'), 2), ((2, 'a'), 1), ((2, 'b'), 1)] :: Dfa Char
          dfa3 = buildDfa 0 [1] [((0, 'a'), 1), ((0, 'b'), 2)] :: Dfa Char
      checkEqual dfa1 "dfa1" dfa2 "dfa2"
      checkEqual dfa1 "dfa1" dfa3 "dfa3"

checkEqual :: Ord c => Dfa c -> String -> Dfa c -> String -> IO ()
checkEqual dfa1 name1 dfa2 name2 = do
    putStrLn $ name1 ++ " and " ++ name2 ++ (getResult dfa1 dfa2 $ dfa1 `dfaEquivalentHkNaive` dfa2)
    putStrLn $ name1 ++ " and " ++ name2 ++ (getResult dfa1 dfa2 $ dfa1 `dfaEquivalentHk` dfa2)

  where getResult dfa1 dfa2 r = if r then " are equal" else " are not equal"
