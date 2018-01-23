module Data.CongruenceClosure
    ( CongruenceClosure
    , empty
    , equate
    , unequate
    , equivalent
    ) where

import Data.CongruenceClosure.Internal

import Data.IntSet                     ( IntSet )
import Data.List                       as List

type Rule = (IntSet, IntSet)

data CongruenceClosure =
    CongruenceClosure [Rule]

empty :: CongruenceClosure
empty = CongruenceClosure []

equate :: CongruenceClosure -> IntSet -> IntSet -> CongruenceClosure
equate relation@(CongruenceClosure rules) iset1 iset2 =
    if equivalent iset1 iset2 relation
        then relation
        else CongruenceClosure ((iset1, iset2) : (iset2, iset1) : rules)

equivalent :: IntSet -> IntSet -> CongruenceClosure -> Bool
equivalent is1 is2 (CongruenceClosure rules) =
    normalForm rules is1 == normalForm rules is2

unequate :: CongruenceClosure -> IntSet -> IntSet -> CongruenceClosure
unequate (CongruenceClosure rules) iset1 iset2 =
    CongruenceClosure $
    List.delete (iset1, iset2) $ List.delete (iset2, iset1) rules
