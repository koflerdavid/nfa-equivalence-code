module Data.CongruenceClosure
    ( CongruenceClosure
    , empty
    , equate
    , equal
    ) where

import qualified Data.IntSet as IS

type Rule = (IS.IntSet, IS.IntSet)

data CongruenceClosure = CongruenceClosure [Rule]

empty :: CongruenceClosure
empty = CongruenceClosure []

equate :: CongruenceClosure -> [Int] -> [Int] -> CongruenceClosure
equate rel is1 is2 = equateIntSets rel (IS.fromList is1) (IS.fromList is2)

equateIntSets :: CongruenceClosure -> IS.IntSet -> IS.IntSet -> CongruenceClosure
equateIntSets relation@(CongruenceClosure rules) iset1 iset2 =
    if intSetsEqual iset1 iset2 relation
    then relation
    else CongruenceClosure ((iset1, iset2) : (iset2, iset1) : rules)

equal :: [Int] -> [Int] -> CongruenceClosure -> Bool
equal is1 is2 rel = intSetsEqual (IS.fromList is1) (IS.fromList is2) rel

intSetsEqual :: IS.IntSet -> IS.IntSet -> CongruenceClosure -> Bool
intSetsEqual is1 is2 rel =
    normalForm rel is1 == normalForm rel is2

normalForm :: CongruenceClosure -> IS.IntSet -> IS.IntSet
normalForm (CongruenceClosure rules) integerSet =
    normalise integerSet rules [] False
  where
    normalise iset [] [] _ =
        iset
    normalise iset [] _toRetry False =
        iset
    normalise iset [] toRetry True =
        normalise iset toRetry [] False
    normalise iset (rule@(left, right) : rs) unmatched retry =
        if left `IS.isSubsetOf` iset
        then normalise (iset `IS.union` right) rs unmatched True
        else normalise iset rs (rule : unmatched) retry
