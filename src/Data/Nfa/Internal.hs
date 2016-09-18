module Data.Nfa.Internal where

import Control.Monad.Trans.RWS.Strict
import qualified Data.Map.Strict as M
import Data.IntSet as IS

type Transitions c = M.Map (Int, Maybe c) IntSet
type NfaState c a = RWS (Transitions c) () IntSet a

transitionTable :: Ord c => [((Int, Maybe c), [Int])] -> Transitions c
transitionTable nfaTransitionFunction = IS.union `M.fromListWith` transitions
  where transitions = Prelude.map (\ (q_and_c, states) -> (q_and_c, IS.fromList states)) nfaTransitionFunction

-- This function should update the state and also consider the epsilon closure
nfaStep :: Ord c => c -> NfaState c ()
nfaStep c = do
  table <- ask
  modify (\qs -> closure table $ step table (closure table qs) (Just c))

-- This function is just supposed to query the table
step :: Ord c => Transitions c -> IntSet -> Maybe c -> IntSet
step table qs c = mergeMap (\q -> M.findWithDefault IS.empty (q, c) table) qs

closure :: Ord c => (Transitions c) -> IntSet -> IntSet
closure transitions states = computeClosure states states
    where computeClosure current visited =
            let epsilonReachable = mergeMap (getEpsilonReachableStates transitions) current
                visited' = visited `IS.union` epsilonReachable
                toVisit = epsilonReachable IS.\\ visited
            in if IS.null toVisit then visited'
                                  else computeClosure toVisit visited'

getEpsilonReachableStates :: Ord c => Transitions c -> Int -> IntSet
getEpsilonReachableStates transitions q = M.findWithDefault IS.empty (q, Nothing) transitions

mergeMap :: (Int -> IntSet) -> IntSet -> IntSet
mergeMap f set = IS.foldr (\x y -> f x `IS.union` y) IS.empty set