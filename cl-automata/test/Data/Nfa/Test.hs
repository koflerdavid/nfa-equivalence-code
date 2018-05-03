{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module:      Data.Nfa.Test
Description: 
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)


-}
module Data.Nfa.Test
    ( subsetOf
    ) where

import Data.Nfa

import Control.Monad   ( forM )
import Data.Foldable
import Test.QuickCheck

instance Arbitrary (Nfa Char) where
    arbitrary = do
        states <- listOf (arbitrary :: Gen Int)
        alphabet <- listOf $ elements (['a' .. 'z'] ++ ['A' .. 'Z'])
        generateNfa states alphabet

generateNfa :: Ord c => [Int] -> [c] -> Gen (Nfa c)
generateNfa states alphabet = do
    acceptingStates <- sublistOf states
    transitionList <-
        forM [(from, c) | from <- states, c <- alphabet] $ \key -> do
            state <- subsetOf states
            return (key, state)
    return $ buildNfa acceptingStates transitionList

subsetOf :: Foldable t => t a -> Gen [a]
subsetOf s = shuffle (Data.Foldable.toList s) >>= sublistOf
