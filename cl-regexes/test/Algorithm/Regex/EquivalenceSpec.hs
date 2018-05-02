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
    ( prop_distinguishAtoms
    , prop_sameRegexesShallBeEquivalent
    , prop_sameRegexesShallHaveNoDifferences
    , spec_differentRegexesShallBeDifferent
    ) where

import CommonInstances             ()

import Algorithm.Regex.Equivalence ( equivalent, getDifferences )
import Data.Regex                  ( Regex(..) )

import Control.Monad               ( forM_ )
import Test.Hspec
import Test.QuickCheck

spec_differentRegexesShallBeDifferent :: Spec
spec_differentRegexesShallBeDifferent = do
    describe "regular expression comparison" $ do
        let cases :: [(Regex Char, Regex Char)] =
                [("a", "b"), ("a", "a*"), ("a", "aa*"), ("ab", "ba")]
        forM_ cases $ \(r1, r2) ->
            it ("should distinguish " ++ show r1 ++ " from " ++ show r2) $ do
                fst (getDifferences r1 r2) `shouldNotBe` []

prop_distinguishAtoms :: Char -> Char -> Property
prop_distinguishAtoms c1 c2 =
    (c1 /= c2) ==>
        let (r1, r2) = (Atom c1, Atom c2)
        -- No assumption is placed on which input symbol is processed first
        in fst (getDifferences r1 r2) === [([c1], Epsilon, Empty)]
            .||. fst (getDifferences r1 r2) === [([c2], Empty, Epsilon)]

prop_sameRegexesShallHaveNoDifferences :: Regex Char -> Property
prop_sameRegexesShallHaveNoDifferences r = fst (getDifferences r r) === []

prop_sameRegexesShallBeEquivalent :: Regex Char -> Bool
prop_sameRegexesShallBeEquivalent r = equivalent r r
