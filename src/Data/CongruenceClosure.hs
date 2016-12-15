module Data.CongruenceClosure
    ( CongruenceClosure
    , empty
    , equate
    , equivalent
    ) where

import Data.CongruenceClosure.Internal

import qualified Data.IntSet as IS

type Rule = (IS.IntSet, IS.IntSet)
data CongruenceClosure = CongruenceClosure [Rule]

empty :: CongruenceClosure
empty = CongruenceClosure []

equate :: CongruenceClosure -> IS.IntSet -> IS.IntSet -> CongruenceClosure
equate relation@(CongruenceClosure rules) iset1 iset2 =
    if equivalent iset1 iset2 relation
    then relation
    else CongruenceClosure ((iset1, iset2) : (iset2, iset1) : rules)

equivalent :: IS.IntSet -> IS.IntSet -> CongruenceClosure -> Bool
equivalent is1 is2 (CongruenceClosure rules) =
    normalForm rules is1 == normalForm rules is2
