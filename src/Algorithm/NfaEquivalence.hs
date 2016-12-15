module Algorithm.NfaEquivalence where

import qualified Data.CongruenceClosure         as CC
import           Data.Nfa

import           Control.Monad.Trans.State.Lazy
import           Data.Equivalence.Monad
import qualified Data.IntSet                    as IS
import           Data.Sequence
import qualified Data.Set                       as S

type NfaStates = IS.IntSet

type Error = String

nfaStatesEquivalentHkNaive :: (Ord c) => Nfa c -> NfaStates -> NfaStates -> Either Error Bool
nfaStatesEquivalentHkNaive nfa set1 set2 =
    return (runEquivM' $ check (viewl $ singleton (set1, set2)))
  where
    check :: ViewL (NfaStates, NfaStates) -> EquivM' s NfaStates Bool
    check EmptyL = return True
    check ((xs, ys) :< ps) = do
        alreadyEqual <- xs `equivalent` ys
        if alreadyEqual
            then check (viewl ps)
            else if (nfa `accepts` IS.toList xs) /= (nfa `accepts` IS.toList ys)
                 then return False
                 else equate xs ys >> check (viewl $ fromList todo >< ps)
      where
        todo :: [(NfaStates, NfaStates)]
        todo = [ (xs `nfaStep'` c, ys `nfaStep'` c)
               | c <- alphabet ]

    alphabet = S.toList (nfaAlphabet nfa)
    nfaStep' = nfaStep nfa

type HkcEquivM a = StateT CC.CongruenceClosure (Either Error) a

nfaStatesEquivalentHkC :: (Ord c) => Nfa c -> NfaStates -> NfaStates -> Either Error Bool
nfaStatesEquivalentHkC nfa set1 set2 =
    evalStateT (check (viewl $ singleton (set1, set2))) CC.empty
  where
    check :: ViewL (NfaStates, NfaStates) -> HkcEquivM Bool
    check EmptyL = return True
    check ((xs, ys) :< ps) = do
        -- TODO: do not only consider the relation, but also the pairs in `ps`. See 3.3
        alreadyEqual <- xs `equivalentM` ys
        if alreadyEqual
            then check (viewl ps)
            else if (nfa `accepts` IS.toList xs) /= (nfa `accepts` IS.toList ys)
                 then return False
                 else equateM xs ys >> check (viewl $ fromList todo >< ps)
      where
        todo :: [(NfaStates, NfaStates)]
        todo = [ (xs `nfaStep'` c, ys `nfaStep'` c)
               | c <- alphabet ]

    alphabet = S.toList (nfaAlphabet nfa)
    nfaStep' = nfaStep nfa

    equivalentM :: NfaStates -> NfaStates -> HkcEquivM Bool
    equivalentM set1 set2 = gets $ \relation -> CC.equivalent set1 set2 relation

    equateM :: NfaStates -> NfaStates -> HkcEquivM ()
    equateM set1 set2 = modify $ \relation -> CC.equate relation set1 set2
