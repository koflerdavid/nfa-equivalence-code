{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module:      Data.QueueTest
Description: Validate properties of queues.
Copyright:   (C) David Kofler
License:     BSD3 (see the LICENSE file in the distribution)

Maintainer:  kofler.david@gmail.com
Stability:   provisional
Portability: portable (Haskell 2010)
-}
module Data.QueueTest where

import qualified Data.Queue      as Q

import           Data.Maybe      ( isJust, isNothing )
import           Data.Semigroup
import           Test.Invariant  ( associative )
import           Test.QuickCheck

prop_emptyQueuesAreEmpty :: Property
prop_emptyQueuesAreEmpty = forAllEmptyQueues Q.null

prop_pushingIncreasesSize :: Property
prop_pushingIncreasesSize =
    forAllEmptyQueues $ \queue ->
        let biggerQueue = Q.push queue (1 :: Int)
        in Q.null queue .&&. not (Q.null biggerQueue)

prop_poppingEmptyQueueIsNotPossible :: Property
prop_poppingEmptyQueueIsNotPossible = forAllEmptyQueues (isNothing . Q.pop)

prop_poppingNonEmptyQueueIsPossible :: Gen Property
prop_poppingNonEmptyQueueIsPossible =
    forAllNonEmptyQueues $ \queue ->
        let result :: Maybe Int = fst <$> Q.pop queue
        in isJust result

prop_singletonQueuesAreEmptyAfterPopping :: Gen Property
prop_singletonQueuesAreEmptyAfterPopping =
    forAllSingletonQueues $ \queue ->
        let result = Q.pop queue
            _content = fmap fst result :: Maybe Int
        in isJust result .&&. fmap (Q.null . snd) result === Just True

prop_fifoQueueReversesInput :: [Int] -> Property
prop_fifoQueueReversesInput contents =
    let filledQueue :: Q.FifoQueue Int = Q.empty `Q.pushAll` contents
        queueContents q = maybe [] (\(a, q') -> a : queueContents q') (Q.pop q)
        extractedContents = queueContents filledQueue
    in (not (null contents) ==> not (Q.null filledQueue)) .&&. extractedContents ===
       reverse contents

prop_fifoQueueMaintainsInputOrder :: [Int] -> Property
prop_fifoQueueMaintainsInputOrder contents =
    let filledQueue :: Q.LifoQueue Int = Q.empty `Q.pushAll` contents
        queueContents q = maybe [] (\(a, q') -> a : queueContents q') (Q.pop q)
        extractedContents = queueContents filledQueue
    in (not (null contents) ==> not (Q.null filledQueue)) .&&. extractedContents === contents

prop_sappendIsAssociativeForFifoQueue ::
       Q.FifoQueue Int -> Q.FifoQueue Int -> Q.FifoQueue Int -> Bool
prop_sappendIsAssociativeForFifoQueue = associative (<>)

prop_sappendIsAssociativeForLifoQueue ::
       Q.LifoQueue Int -> Q.LifoQueue Int -> Q.LifoQueue Int -> Bool
prop_sappendIsAssociativeForLifoQueue = associative (<>)

-- | Checks whether a given property holds for all empty queues.
forAllEmptyQueues ::
       Testable prop
    => (forall q. Q.Queue q => q a -> prop)
    -> Property
forAllEmptyQueues propOf =
    let emptyFifoQueue = Q.empty :: Q.FifoQueue a
        emptyLifoQueue = Q.empty :: Q.LifoQueue a
    in propOf emptyFifoQueue .&&. propOf emptyLifoQueue

-- | Checks whether a given property holds for all nonempty queues.
forAllNonEmptyQueues ::
       forall a prop. (Arbitrary a, Testable prop)
    => (forall q. Q.Queue q => q a -> prop)
    -> Gen Property
forAllNonEmptyQueues propOf = do
    fifoQueue :: Q.FifoQueue a <- Q.pushAll Q.empty <$> listOf1 (arbitrary :: Gen a)
    lifoQueue :: Q.LifoQueue a <- Q.pushAll Q.empty <$> listOf1 (arbitrary :: Gen a)
    return $ propOf fifoQueue .&&. propOf lifoQueue

-- | Checks whether a given property holds for all queues.
forAllQueues ::
       forall a prop. (Arbitrary a, Testable prop)
    => (forall q. Q.Queue q => q a -> prop)
    -> Gen Property
forAllQueues propOf = do
    fifoQueue :: Q.FifoQueue a <- Q.pushAll Q.empty <$> listOf (arbitrary :: Gen a)
    lifoQueue :: Q.LifoQueue a <- Q.pushAll Q.empty <$> listOf (arbitrary :: Gen a)
    return $ propOf fifoQueue .&&. propOf lifoQueue

-- | Checks whether a given property holds for all singleton queues.
forAllSingletonQueues ::
       forall a prop. (Arbitrary a, Testable prop)
    => (forall q. Q.Queue q => q a -> prop)
    -> Gen Property
forAllSingletonQueues propOf = do
    fifoQueue :: Q.FifoQueue a <- Q.singleton <$> (arbitrary :: Gen a)
    lifoQueue :: Q.LifoQueue a <- Q.singleton <$> (arbitrary :: Gen a)
    return $ propOf fifoQueue .&&. propOf lifoQueue

instance Arbitrary a => Arbitrary (Q.FifoQueue a) where
    arbitrary = Q.pushAll Q.empty <$> listOf (arbitrary :: Gen a)

instance Arbitrary a => Arbitrary (Q.LifoQueue a) where
    arbitrary = Q.pushAll Q.empty <$> listOf (arbitrary :: Gen a)
