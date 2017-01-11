{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.NfaEquivalence where

import qualified Data.CongruenceClosure         as CC
import           Data.Nfa
import           Data.Queue                     as Q

import           Control.Monad.Trans.State.Lazy
import           Data.Equivalence.Monad
import qualified Data.IntSet                    as IS
import qualified Data.Set                       as S

type NfaStates = IS.IntSet

type Error = String

type Constraint c = ([c], NfaStates, NfaStates)

nfaStatesEquivalentHkNaive :: forall c. (Ord c) => Nfa c -> NfaStates -> NfaStates -> Bool
nfaStatesEquivalentHkNaive nfa set1 set2 =
    runEquivM' $ check (Q.singleton ([], set1, set2))
  where
    check :: FifoQueue (Constraint c) -> EquivM' s NfaStates Bool
    check queue = (flip . maybe) (return True) (Q.pop queue) $
        \(constraint@(w, xs, ys), queue') -> do
            alreadyEqual <- xs `equivalent` ys
            if alreadyEqual
                then check queue'
                else if (nfa `accepts` IS.toList xs) /= (nfa `accepts` IS.toList ys)
                     then return False
                     else equate xs ys >> check (queue' `Q.pushAll` todo constraint)
      where
        todo (w, xs, ys) = [ (c : w, xs `nfaStep'` c, ys `nfaStep'` c)
                           | c <- alphabet ]

    alphabet = S.toList (nfaAlphabet nfa)
    nfaStep' = nfaStep nfa

type HkcEquivM = State CC.CongruenceClosure

nfaStatesEquivalentHkC :: forall c. (Ord c) => Nfa c -> NfaStates -> NfaStates -> Bool
nfaStatesEquivalentHkC nfa set1 set2 =
    evalState (check (Q.singleton ([], set1, set2))) CC.empty
  where
    check :: FifoQueue (Constraint c) -> HkcEquivM Bool
    check queue = (flip . maybe) (return True) (Q.pop queue) $
        \(constraint@(w, xs, ys), queue') -> do
            -- TODO: do not only consider the relation, but also the pairs in `ps`. See 3.3
            alreadyEqual <- xs `equivalentM` ys
            if alreadyEqual
                then check queue'
                else if (nfa `accepts` IS.toList xs) /= (nfa `accepts` IS.toList ys)
                     then return False
                     else equateM xs ys >> check (queue' `Q.pushAll` todo constraint)
      where
        todo (w, xs, ys) = [ (c : w, xs `nfaStep'` c, ys `nfaStep'` c)
                           | c <- alphabet ]

    alphabet = S.toList (nfaAlphabet nfa)
    nfaStep' = nfaStep nfa

    equivalentM :: NfaStates -> NfaStates -> HkcEquivM Bool
    equivalentM s1 s2 = gets $ \relation -> CC.equivalent s1 s2 relation

    equateM :: NfaStates -> NfaStates -> HkcEquivM ()
    equateM s1 s2 = modify $ \relation -> CC.equate relation s1 s2
