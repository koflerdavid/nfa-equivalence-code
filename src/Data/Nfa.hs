{-# LANGUAGE ScopedTypeVariables #-}

module Data.Nfa
  (
    Nfa(..)
    , runNfa
    , accepts
  ) where

import Control.Monad
import Control.Monad.Trans.RWS.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Nfa c =
  Nfa { nfaAlphabet :: [c]
      , nfaStates :: [Int]
      , nfaInitialStates :: [Int]
      , nfaFinalStates :: [Int]
      , nfaTransitionFunction :: [((Int, Maybe c), [Int])]
      }
      deriving (Eq, Show)

runNfa :: (Ord c, Show c) => Nfa c -> [c] -> [Int]
runNfa nfa input =
  let table = transitionTable nfa
      initialStates = S.fromList (nfaInitialStates nfa)
      clInitialStates = closure table initialStates
      finalStates = fst $ execRWS (forM_ input nfaStep) table clInitialStates
  in S.toList $ finalStates

transitionTable :: Ord c => Nfa c -> Transitions c
transitionTable nfa = S.union `M.fromListWith` transitions
  where transitions = map (\ (q_and_c, states) -> (q_and_c, S.fromList states)) (nfaTransitionFunction nfa)

accepts :: Nfa c -> [Int] -> Bool
accepts nfa possibleStates = not . S.null $ S.fromList (nfaFinalStates nfa) `S.intersection` S.fromList possibleStates

type Transitions c = M.Map (Int, Maybe c) (S.Set Int)
type NfaState c a = RWS (Transitions c) () (S.Set Int) a

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
