{- |
Module:      Data.Regex.InstancesTest
Description: 
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)


-}

module Data.Regex.InstancesTest
(
  prop_applicativeComposes
  , prop_applicativePureIsIdempotent
  , prop_applicativeSequenceInterchanges
  , prop_applicativeSupportsHomomorphisms
  , prop_bindIsAssociative
  , prop_fmapIdIsIdempotent
  , prop_fmapIsEndomorphism
  , prop_leftNeutralElementForBind
  , prop_rightNeutralElementForBind
) where

import Data.Regex

import Test.Invariant
import Test.QuickCheck

prop_fmapIdIsIdempotent :: Regex Char -> Bool
prop_fmapIdIsIdempotent = idempotent (fmap id)

prop_fmapIsEndomorphism :: (Int -> Double) -> (Char -> Int) -> Regex Char -> Bool
prop_fmapIsEndomorphism f g = regexFmap (f . g) <=> (regexFmap f . fmap g)
  where
    regexFmap :: (a -> b) -> Regex a -> Regex b
    regexFmap = fmap

prop_applicativePureIsIdempotent :: Regex Char -> Bool
prop_applicativePureIsIdempotent = idempotent (pure id <*>)

prop_applicativeComposes :: (Int -> Double) -> (Char -> Int) -> Regex Char -> Bool
prop_applicativeComposes x y = left (pure x) (pure y) <=> right (pure x) (pure y)
    where
        left :: Regex (b -> c) -> Regex (a -> b) -> Regex a -> Regex c
        left u v w = pure (.) <*> u <*> v <*> w

        right :: Regex (b -> c) -> Regex (a -> b) -> Regex a -> Regex c
        right u v w = u <*> (v <*> w)

prop_applicativeSupportsHomomorphisms :: (Char -> Int) -> Char -> Bool
prop_applicativeSupportsHomomorphisms f = ((regexPure f <*>) . regexPure ) <=> (regexPure . f)
    where
        regexPure :: a -> Regex a
        regexPure = pure

prop_applicativeSequenceInterchanges :: (Char -> Int) -> Char -> Bool
prop_applicativeSequenceInterchanges u = ((rU <*>) . pure) <=> (\y -> pure ($ y) <*> rU)
    where
        rU  :: Regex (Char -> Int)
        rU = pure u

prop_leftNeutralElementForBind :: Int -> (Int -> Regex Char) -> Property
prop_leftNeutralElementForBind c fR = (return c >>= fR) === fR c

prop_rightNeutralElementForBind :: Regex Char -> Property
prop_rightNeutralElementForBind r = (r >>= return) === r

prop_bindIsAssociative :: Regex Int -> (Int -> Regex Char) -> (Char -> Regex Int) -> Property
prop_bindIsAssociative r k h = (r >>= (\x -> k x >>= h)) === ((r >>= k) >>= h)
