module Algorithm.NfaEquivalence where

import           Data.Nfa

import qualified Data.IntSet            as IS
import           Data.Equivalence.Monad
import           Data.Sequence
import qualified Data.Set                as S

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
