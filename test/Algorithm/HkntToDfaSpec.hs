module Algorithm.HkntToDfaSpec (main, spec) where

import Algorithm.HkntToDfa
import Data.Dfa
import Language.Automata.HkntParser

import Control.Monad
import Data.IntSet as IS
import Data.Map as M
import Data.Set as S
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec = do
  describe "the DFA compiler" $ do
    forM_ cases $ \ (input, expected) -> do
      it ("should compile " ++ show input) $ do
        compiledDfa input `shouldBe` Right expected
          where
            compiledDfa input = do
              Result transitions acceptingStates _ <- parseHknt input
              compileHkntToDfa transitions acceptingStates


cases = [
  ("accept: a b", Dfa {
        dfaStates = IS.fromList [0, 1, 2, 3]
      , dfaInitialState = 2
      , dfaFinalStates = IS.fromList [0, 1]
      , dfaErrorState = 3
      , dfaTransitionFunction = M.empty
      } )
  ]
