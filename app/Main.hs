module Main where

import Data.Dfa
import Data.Nfa
import Data.Regex

import Algorithm.RegexCompiler

main :: IO ()
main = do
  let dfa = complete 3 $ (Dfa "ab" [0,1,2,3] 0 [2] [((0,'a'), 1), ((1, 'b'), 2), ((2, 'a'), 1)]) :: Dfa Int Char
  putStrLn $ show (runDfa dfa "ab")
  putStrLn $ show (runDfa dfa "aba")
  putStrLn $ show (runDfa dfa "abb")
  putStrLn $ show (runDfa dfa "abab")
  putStrLn $ show (runDfa dfa "aabbaa")
  putStrLn $ show (runDfa dfa "")
  putStrLn $ show (dfa == (fst . transformToIntegerStates) dfa)

  let nfa = compileRegex (Asterisk (Alternative [Atom 'a', Atom 'b']))
  putStrLn $ show nfa
  putStrLn $ show (runNfa nfa "a")
  putStrLn $ show (runNfa nfa "b")
  putStrLn $ show (runNfa nfa "d")
  putStrLn $ show (runNfa nfa "ab")
  putStrLn $ show (runNfa nfa "")
