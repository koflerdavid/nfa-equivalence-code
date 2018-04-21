{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module:      Algorithm.Regex.EquivalenceSpec
Description:
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)
-}
module Algorithm.Regex.EquivalenceSpec
    ( prop_sameRegexesShallHaveNoDifferences
    , spec_differentRegexesShallBeDifferent
    ) where

import CommonInstances             ()

import Algorithm.Regex.Equivalence ( getDifferences )
import Data.Regex                  ( Regex )

import Control.Monad               ( forM_ )
import Test.Hspec

spec_differentRegexesShallBeDifferent :: Spec
spec_differentRegexesShallBeDifferent = do
    describe "regular expression comparison" $ do
        let cases :: [(Regex Char, Regex Char)] =
                [("a", "b"), ("a", "a*"), ("a", "aa*"), ("ab", "ba")]
        forM_ cases $ \(r1, r2) ->
            it ("should distinguish " ++ show r1 ++ " from " ++ show r2) $ do
                fst (getDifferences r1 r2) `shouldNotBe` []

prop_sameRegexesShallHaveNoDifferences :: Regex Char -> Bool
prop_sameRegexesShallHaveNoDifferences r = fst (getDifferences r r) == []
