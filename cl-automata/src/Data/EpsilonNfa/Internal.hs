module Data.EpsilonNfa.Internal where

import Control.Monad.Trans.RWS.Strict
import Data.IntSet                    as ISet
import Data.Map                       ( Map, findWithDefault )

type Transitions c = Map (Int, Maybe c) IntSet

type NfaState c a = RWS (Transitions c) () IntSet a

-- This function should update the state and also consider the epsilon closure
enfaStep :: Ord c => c -> NfaState c ()
enfaStep c = do
    table <- ask
    modify (\qs -> closure table $ step table (closure table qs) (Just c))

-- This function is just supposed to query the table
step :: Ord c => Transitions c -> IntSet -> Maybe c -> IntSet
step table qs c = mergeMap (\q -> findWithDefault ISet.empty (q, c) table) qs

closure :: Ord c => Transitions c -> IntSet -> IntSet
closure transitions states = computeClosure states states
  where
    computeClosure current visited =
        if ISet.null toVisit
            then visited'
            else computeClosure toVisit visited'
      where
        epsilonReachable =
            mergeMap (getEpsilonReachableStates transitions) current
        visited' = visited `ISet.union` epsilonReachable
        toVisit = epsilonReachable ISet.\\ visited

getEpsilonReachableStates :: Ord c => Transitions c -> Int -> IntSet
getEpsilonReachableStates transitions q =
    findWithDefault ISet.empty (q, Nothing) transitions

mergeMap :: (Int -> IntSet) -> IntSet -> IntSet
mergeMap f = ISet.foldr (\x y -> f x `ISet.union` y) ISet.empty
