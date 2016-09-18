module Data.Nfa.Internal where

import Control.Monad.Trans.RWS.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Transitions c = M.Map (Int, Maybe c) (S.Set Int)
type NfaState c a = RWS (Transitions c) () (S.Set Int) a

transitionTable :: Ord c => [((Int, Maybe c), [Int])] -> Transitions c
transitionTable nfaTransitionFunction = S.union `M.fromListWith` transitions
  where transitions = map (\ (q_and_c, states) -> (q_and_c, S.fromList states)) nfaTransitionFunction

nfaStep :: Ord c => c -> NfaState c ()
nfaStep c = do
  table <- ask
  modify (\qs -> closure table $ step table qs (Just c))

step :: Ord c => Transitions c -> S.Set Int -> Maybe c -> S.Set Int
step table qs c = mergeMap (\q -> M.findWithDefault S.empty (q, c) table) qs

closure :: Ord c => (Transitions c) -> S.Set Int -> S.Set Int
closure transitions states = computeClosure states S.empty
    where computeClosure current visited =
            let epsilonReachable = mergeMap (getEpsilonReachableStates transitions) current
                visited' = visited `S.union` epsilonReachable
                toVisit = epsilonReachable S.\\ visited
            in {-unsafeInlineIO (putStrLn $ show toVisit) `seq`-} if S.null toVisit then visited'
                                                                                    else computeClosure toVisit visited'

getEpsilonReachableStates :: Ord c => Transitions c -> Int -> S.Set Int
getEpsilonReachableStates transitions q = M.findWithDefault S.empty (q, Nothing) transitions

mergeMap :: (Ord a, Ord b)  => (a -> S.Set b) -> S.Set a -> S.Set b
mergeMap f set = S.foldr S.union S.empty $ S.map f set
