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

data Nfa q c =
  Nfa { nfaAlphabet :: [c]
      , nfaStates :: [q]
      , nfaInitialStates :: [q]
      , nfaFinalStates :: [q]
      , nfaTransitionFunction :: [((q, Maybe c), [q])]
      }
      deriving (Eq, Show)

runNfa :: (Ord c, Ord q, Show c, Show q) => Nfa q c -> [c] -> [q]
runNfa nfa input =
  let table = transitionTable nfa
      initialStates = S.fromList (nfaInitialStates nfa)
      clInitialStates = closure table initialStates
      finalStates = fst $ execRWS (forM_ input nfaStep) table clInitialStates
  in S.toList $ finalStates

transitionTable :: (Ord q, Ord c) => Nfa q c -> Transitions q c
transitionTable nfa = S.union `M.fromListWith` transitions
  where transitions = map (\ (q_and_c, states) -> (q_and_c, S.fromList states)) (nfaTransitionFunction nfa)

accepts :: Ord q => Nfa q c -> [q] -> Bool
accepts nfa possibleStates = not . S.null $ S.fromList (nfaFinalStates nfa) `S.intersection` S.fromList possibleStates

type Transitions q c = M.Map (q, Maybe c) (S.Set q)
type NfaState q c a = RWS (Transitions q c) () (S.Set q) a

nfaStep :: (Ord c, Ord q, Show q) => c -> NfaState q c ()
nfaStep c = do
  table <- ask
  modify (\qs -> closure table $ step table qs (Just c))

step :: (Ord c, Ord q) => Transitions q c -> S.Set q -> Maybe c -> S.Set q
step table qs c = mergeMap (\q -> M.findWithDefault S.empty (q, c) table) qs

closure :: (Ord c, Ord q, Show q) => (Transitions q c) -> S.Set q -> S.Set q
closure transitions states = computeClosure states S.empty
    where computeClosure current visited =
            let epsilonReachable = mergeMap (getEpsilonReachableStates transitions) current
                visited' = visited `S.union` epsilonReachable
                toVisit = epsilonReachable S.\\ visited
            in {-unsafeInlineIO (putStrLn $ show toVisit) `seq`-} if S.null toVisit then visited'
                                                                                    else computeClosure toVisit visited'

getEpsilonReachableStates :: (Ord c, Ord q) => Transitions q c -> q -> S.Set q
getEpsilonReachableStates transitions q = M.findWithDefault S.empty (q, Nothing) transitions

mergeMap :: (Ord a, Ord b)  => (a -> S.Set b) -> S.Set a -> S.Set b
mergeMap f set = S.foldr S.union S.empty $ S.map f set
