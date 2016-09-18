module NfaSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Nfa
import Data.Regex
import Algorithm.RegexCompiler

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()
