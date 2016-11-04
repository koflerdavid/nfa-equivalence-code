module Data.Nfa
    ( Nfa(..)
    , nfaAlphabet
    , nfaStates
    , buildNfa
    , runNfa
    , accepts
    ) where

import           Data.Foldable as Foldable
import           Data.IntSet   as ISet
import           Data.List     as List
import           Data.Map      as Map
import           Data.Set      as Set

data Nfa c = Nfa { nfaAcceptingStates :: ISet.IntSet
                 , nfaTransitions     :: Map.Map (Int, c) ISet.IntSet
                 }
    deriving (Eq, Show)

-- | Build an NFA out of the given transitions.
-- The main advantage over the constructor is that the input can be made using lists.
buildNfa :: (Ord c) => [Int] -> [((Int, c), [Int])] -> Nfa c
buildNfa acceptingStates
    = Nfa (ISet.fromList acceptingStates)
    . Map.fromListWith ISet.union
    . List.map (mapSnd ISet.fromList) -- Use sets to represent target states

nfaAlphabet :: (Ord c) => Nfa c -> Set.Set c
nfaAlphabet
    = Set.map snd -- Only extract the characters.
    . Map.keysSet
    . nfaTransitions

nfaStates :: Nfa c -> ISet.IntSet
nfaStates nfa = nfaAcceptingStates nfa `ISet.union` transitionStates nfa
  where
    transitionStates = ISet.unions
        . List.map (\((q, _), qs) -> q `ISet.insert` qs) -- Collect the states
            . Map.toAscList
                . nfaTransitions

runNfa :: (Ord c, Foldable t) => Nfa c -> [Int] -> t c -> [Int]
runNfa nfa initialStates =
    ISet.toList . Foldable.foldl (nfaStep nfa) (ISet.fromList initialStates)

nfaStep :: (Ord c) => Nfa c -> IntSet -> c -> IntSet
nfaStep nfa qs input = ISet.unions
                     $ List.map (\q -> Map.findWithDefault ISet.empty (q, input) transitions)
                     $ ISet.toList qs
  where
    transitions = nfaTransitions nfa

accepts :: Nfa c -> [Int] -> Bool
accepts nfa possibleStates =
    not . ISet.null $ nfaAcceptingStates nfa `ISet.intersection` ISet.fromList possibleStates

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
