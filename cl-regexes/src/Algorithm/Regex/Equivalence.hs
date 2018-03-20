{-# LANGUAGE TupleSections #-}

module Algorithm.Regex.Equivalence
    ( Algorithm.Regex.Equivalence.equivalent
    , getDifferences
    , Witness
    ) where

import Algorithm.Regex.Derivation
import Data.Queue                      as Q
import Data.Regex

import Control.Arrow                   ( (***) )
import Control.Monad.Trans.Class       ( lift )
import Control.Monad.Trans.Writer.Lazy
import Data.Equivalence.Monad          as Equiv
import Data.Foldable
import Data.List                       as List
import Data.Sequence                   as Seq
import Data.Set                        as Set

equivalent :: (Ord c) => Regex c -> Regex c -> Bool
equivalent regex1 regex2 = List.null (getDifferences regex1 regex2)

type Witness c = ([c], Regex c, Regex c)

type Trace c = [(Bool, Witness c)]

-- | Check equality of two regexes and produce witnesses for strings which one regex accepts and
-- the other doesn't. These are not all strings for which they behave differently, but the shortest.
getDifferences :: (Ord c) => Regex c -> Regex c -> ([Witness c], Trace c)
getDifferences regex1 regex2 =
    let initialConstraint = Q.singleton ([], regex1, regex2)
        -- First, check the constraints
        -- Then, extract the differences which were found
        witnesses =
            execWriter $ runEquivT' (check combinedAlphabet initialConstraint)
        -- Then, convert the Seq to a list.
        -- For efficiency reasons the words which could produce the differences were stored in
        -- reverse. Undoing that is the last step.
    in (List.map reverseWords . Data.Foldable.toList) *** Data.Foldable.toList $ witnesses
  where
    combinedAlphabet = Set.toList $ alphabet regex1 `Set.union` alphabet regex2
    reverseWords (w, r1, r2) = (List.reverse w, r1, r2)

check ::
       Ord c
    => [c]
    -> FifoQueue (Witness c)
    -> EquivT' s (Regex c) (Writer (Seq (Witness c), Seq (Bool, Witness c))) ()
check combinedAlphabet queue =
    (flip . maybe) (return ()) (Q.pop queue) $ \(constraint@(w, r1, r2), queue') -> do
        alreadyEqual <- Equiv.equivalent r1 r2
        if alreadyEqual || r1 == r2
            then do
                Equiv.equate r1 r2
                skipConstraint constraint
                check combinedAlphabet queue'
            else do
                traceConstraint constraint
                if matchesEmptyWord r1 /= matchesEmptyWord r2
                    then do
                        recordMismatch constraint
                        -- At this point, the algorithm could actually stop
                        check combinedAlphabet queue'
                    else do
                        Equiv.equate r1 r2
                        let makeConstraint c = (c : w, derive c r1, derive c r2)
                            constraints = List.map makeConstraint combinedAlphabet
                        check combinedAlphabet (queue' `Q.pushAll` constraints)
  where
    recordMismatch = lift . tell . (, Seq.empty) . Seq.singleton
    traceConstraint = lift . tell . (Seq.empty, ) . Seq.singleton . (False, )
    skipConstraint = lift . tell . (Seq.empty, ) . Seq.singleton . (True, )
