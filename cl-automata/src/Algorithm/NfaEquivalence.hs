{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Algorithm.NfaEquivalence
    ( Constraint
    , nfaStatesDifferencesHkC
    , nfaStatesEquivalentHk
    , nfaStatesEquivalentHkC
    ) where

import qualified Data.CongruenceClosure          as CC
import           Data.Nfa
import           Data.Queue                      as Q

import           Control.Monad.Trans.Class       ( lift )
import           Control.Monad.Trans.RWS.Lazy    as RWS
import           Control.Monad.Trans.Writer.Lazy as Writer
import           Data.Equivalence.Monad
import           Data.Foldable                   ( toList )
import qualified Data.IntSet                     as ISet
import           Data.Maybe                      ( isNothing )
import           Data.Sequence                   as Seq
import qualified Data.Set                        as Set

type NfaStates = ISet.IntSet

type Constraint c = ([c], NfaStates, NfaStates)

nfaStatesEquivalentHk :: (Ord c) => Nfa c -> NfaStates -> NfaStates -> Bool
nfaStatesEquivalentHk nfa set1 set2 =
    let (maybeWitness, _) = nfaStatesDifferencesHk nfa set1 set2
    in isNothing maybeWitness

type HkEquivM s c = EquivT' s NfaStates (Writer (Seq (Bool, Constraint c)))

-- | Find an input word which disproves the equality of the given sets of NFA states,
-- Also the set of possible states the automata are in (after reading the input word) is returned.
-- For each constraint to check a trace is produced.
-- The flag in the traces indicates whether that check was redundant, and thus was skipped.
-- This implementation uses a Union-Find data structure.
nfaStatesDifferencesHk ::
       forall c. (Ord c)
    => Nfa c
    -> NfaStates
    -> NfaStates
    -> (Maybe (Constraint c), [(Bool, Constraint c)])
nfaStatesDifferencesHk nfa set1 set2 =
    let (witness, traces) =
            runWriter $ runEquivT' $ check (Q.singleton ([], set1, set2))
    in (witness, toList traces)
  where
    check :: FifoQueue (Constraint c) -> HkEquivM s c (Maybe (Constraint c))
    check queue =
        (flip . maybe) (return Nothing) (Q.pop queue) $ \(constraint@(_w, xs, ys), queue') -> do
            alreadyEqual <- xs `equivalent` ys
            if alreadyEqual
                then skip constraint >> check queue'
                else do
                    trace constraint
                    if (nfa `accepts` ISet.toList xs) /= (nfa `accepts` ISet.toList ys)
                        then return (Just constraint)
                        else do
                            equate xs ys
                            check (queue' `Q.pushAll` todo constraint)
      where
        todo (w, xs, ys) = [(w ++ [c], xs `nfaStep'` c, ys `nfaStep'` c) | c <- alphabet]
    alphabet = Set.toList (nfaAlphabet nfa)
    nfaStep' = nfaStep nfa
    trace :: Constraint c -> HkEquivM s c () -- Necessary to help the type checker
    trace = lift . Writer.tell . Seq.singleton . (False, )
    skip :: Constraint c -> HkEquivM s c () -- Necessary to help the type checker
    skip = lift . Writer.tell . Seq.singleton . (True, )

type HkcM c = RWS () (Seq (Bool, Constraint c)) CC.CongruenceClosure

nfaStatesEquivalentHkC :: (Ord c) => Nfa c -> NfaStates -> NfaStates -> Bool
nfaStatesEquivalentHkC nfa set1 set2 =
    let (maybeWitness, _) = nfaStatesDifferencesHkC nfa set1 set2
    in isNothing maybeWitness

-- | Find an input word which disproves the equality of the given sets of NFA states,
-- Also the set of possible states the automata are in (after reading the input word) is returned.
-- For each constraint to check a trace is produced.
-- The flag in the traces indicates whether that check was redundant, and thus was skipped.
-- This implementation uses a set rewriting datastructure to implement the congruence closure.
nfaStatesDifferencesHkC ::
       forall c. (Ord c)
    => Nfa c
    -> NfaStates
    -> NfaStates
    -> (Maybe (Constraint c), [(Bool, Constraint c)])
nfaStatesDifferencesHkC nfa set1 set2 =
    let (witness, traces) = evalRWS (check (Q.singleton ([], set1, set2))) () CC.empty
    in (witness, toList traces)
  where
    check :: FifoQueue (Constraint c) -> HkcM c (Maybe (Constraint c))
    check queue =
        (flip . maybe) (return Nothing) (Q.pop queue) $ \(constraint@(_w, xs, ys), queue')
            -- TODO: do not only consider the relation, but also the pairs in `ps`. See 3.3
         -> do
            alreadyEqual <- xs `equivalentM` ys
            if alreadyEqual
                then skip constraint >> check queue'
                else do
                    trace constraint
                    if (nfa `accepts` ISet.toList xs) /= (nfa `accepts` ISet.toList ys)
                        then return (Just constraint)
                        else do
                            equateM xs ys
                            check (queue' `Q.pushAll` todo constraint)
      where
        todo (w, xs, ys) = [(w ++ [c], xs `nfaStep'` c, ys `nfaStep'` c) | c <- alphabet]

    alphabet = Set.toList (nfaAlphabet nfa)

    nfaStep' = nfaStep nfa

    equivalentM :: NfaStates -> NfaStates -> HkcM c Bool
    equivalentM s1 s2 = gets $ \relation -> CC.equivalent s1 s2 relation

    equateM :: NfaStates -> NfaStates -> HkcM c ()
    equateM s1 s2 = modify $ \relation -> CC.equate relation s1 s2

    trace = RWS.tell . Seq.singleton . (False, )

    skip = RWS.tell . Seq.singleton . (True, )
