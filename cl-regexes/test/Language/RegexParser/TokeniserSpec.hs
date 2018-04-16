{-# LANGUAGE OverloadedStrings #-}

module Language.RegexParser.TokeniserSpec
    ( spec_regexTokeniser
    ) where

import Language.RegexParser.Class
import Language.RegexParser.Tokeniser

import Control.Monad                  ( forM_ )
import Test.Hspec
import Text.Parsec

spec_regexTokeniser :: Spec
spec_regexTokeniser = do
    forM_ tokenCases $ \(input, expected) -> do
        it ("should parse " ++ show input ++ " to " ++ show expected) $ do
            parse regexToken "textcase" input `shouldBe` Right expected
  where
    tokenCases = [("a", CharToken 'a')]
