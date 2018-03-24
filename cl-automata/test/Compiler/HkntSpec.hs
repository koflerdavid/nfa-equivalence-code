{-# LANGUAGE OverloadedStrings #-}

module Compiler.HkntSpec
    ( main
    , spec
    ) where

import           Compiler.Hknt
import           Data.Dfa
import           Language.Automata.HkntParser

import           Control.Monad
import           Data.Bifunctor               ( first )
import qualified Data.Text                    as T
import           Test.Hspec

main :: IO ()
main = hspec spec

data Error = E1 String | E2 HkntCompileError
    deriving (Show)

spec :: Spec
spec = do
    describe "the DFA compiler" $ do
        forM_ cases $ \(input, expected) -> do
            it ("should compile " ++ show input) $ do
                compiledDfa input `shouldBe` Right expected
  where
    compiledDfa input = do
        Result transitions acceptingStates _ <- parseHknt input
        fst <$> first show (compileHkntToDfa transitions acceptingStates)

cases :: [(T.Text, Dfa Char)]
cases = [("accept: a b", buildDfaUnsafe [0, 1] [])]
