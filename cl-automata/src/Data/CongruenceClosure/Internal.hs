module Data.CongruenceClosure.Internal where

import Data.IntSet

normalForm :: [(IntSet, IntSet)] -> IntSet -> IntSet
normalForm rules integerSet = normalise integerSet rules [] False
  where
    normalise iset [] [] _ = iset
    normalise iset [] _toRetry False = iset
    normalise iset [] toRetry True = normalise iset toRetry [] False
    normalise iset (rule@(left, right):rs) unmatched retry =
        if left `isSubsetOf` iset
            then normalise (iset `union` right) rs unmatched True
            else normalise iset rs (rule : unmatched) retry
