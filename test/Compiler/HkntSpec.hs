module Compiler.HkntSpec
    ( main
    , spec
    ) where

import Compiler.Hknt
import Data.Dfa
import Language.Automata.HkntParser

import Control.Monad
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "the DFA compiler" $ do
        forM_ cases $
            \(input, expected) -> do
                it ("should compile " ++ show input) $ do
                    compiledDfa input `shouldBe` Right expected
  where
    compiledDfa input = do
        Result transitions acceptingStates _ <- parseHknt input
        fmap fst $ compileHkntToDfa transitions acceptingStates

cases :: [(String, Dfa Char)]
cases = [ ("accept: a b", buildDfaUnsafe [ 0, 1 ] []) ]
