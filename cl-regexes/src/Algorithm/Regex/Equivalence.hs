module Algorithm.Regex.Equivalence
    ( Algorithm.Regex.Equivalence.equivalent
    , getDifferences
    , Witness
    ) where

import Algorithm.AutomataMerge       ( mergeDfa )
import Algorithm.DfaEquivalence      ( dfaStatesDifferencesHk )
import Algorithm.Regex.DfaConversion ( toDfa )
import Data.Regex                    ( Regex )

import Data.Bifunctor                ( bimap, second )
import Data.Bimap                    as Bimap
import Data.List                     as List
import Data.Maybe                    ( maybeToList )

equivalent :: Ord c => Regex c -> Regex c -> Bool
equivalent regex1 regex2 = List.null (getDifferences regex1 regex2)

type Witness c = ([c], Regex c, Regex c)

type Trace c = [(Bool, Witness c)]

-- | Check equality of two regexes and produce witnesses for strings which one regex accepts and
-- the other doesn't. These are not all strings for which they behave differently, but the shortest.
getDifferences :: Ord c => Regex c -> Regex c -> ([Witness c], Trace c)
getDifferences regex1 regex2 =
    let (dfa1, stateToRegex1) = toDfa regex1
        (dfa2, stateToRegex2) = toDfa regex2
        (toMergedState, mergedAutomaton) = mergeDfa dfa1 dfa2
        mergedStateToRegex2 = Bimap.mapR toMergedState stateToRegex2
        (state1, state2) = (stateToRegex1 Bimap.! regex1, mergedStateToRegex2 Bimap.! regex2)
        constraintToWitness (word, cState1, cState2) =
            (word, stateToRegex1 Bimap.!> cState1, mergedStateToRegex2 Bimap.!> cState2)
    in bimap
           (maybeToList . fmap constraintToWitness)
           (List.map (second constraintToWitness))
           (dfaStatesDifferencesHk mergedAutomaton state1 state2)
