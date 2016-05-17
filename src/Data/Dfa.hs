module Data.Dfa
  (
    Dfa(..),
    accepted,
    buildDfa,
    runDfa,
    transformToIntegerStates
  ) where

import Control.Monad
import Control.Monad.Trans.RWS.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

data Dfa q c =
  Dfa { dfaAlphabet :: [c]
      , dfaStates :: [q]
      , dfaInitialState :: q
      , dfaFinalStates :: [q]
      , dfaTransitionFunction :: [((q, c), q)]
      }
      deriving (Eq, Show)

buildDfa :: (Ord c, Ord q) => q -> [q] -> q -> [((q, c), q)] -> Dfa q c
buildDfa initialState finalStates errorState transitions =
  let statesFromTransitions = S.unions $ map (\((q, _c), q') -> S.fromList [q, q']) transitions
      otherStates = S.fromList $ initialState : errorState : finalStates
      states = statesFromTransitions `S.union` otherStates
      alphabet = S.fromList $ map (\((_q, c), _q') -> c) transitions
      transitions' = complete errorState alphabet states (M.fromList transitions)
  in Dfa (S.toList alphabet) (S.toList states) initialState finalStates (M.toList transitions')

complete :: (Ord c, Ord q) => q -> Set c -> Set q -> Map (q, c) q -> Map (q, c) q
complete errorState alphabet states transitionTable =
  let defaultTransitions = [((q, c), errorState) | q <- S.elems states, c <- S.elems alphabet]
  in M.union transitionTable (M.fromList defaultTransitions)

runDfa :: (Ord c, Ord q) => Dfa q c -> [c] -> Either String q
runDfa dfa input =
  let transitionTable = M.fromList (dfaTransitionFunction dfa)
  in case execRWS (forM_ input dfaStep) transitionTable (Just (dfaInitialState dfa)) of
    (Nothing, _) -> Left "Encountered an undefined state transition"
    (Just q, _) -> Right q

accepted :: Eq q => Dfa q c -> q -> Bool
accepted dfa q = q `elem` dfaFinalStates dfa

type Transitions q c = (M.Map (q, c) q)
type DfaState q c a = RWS (Transitions q c) () (Maybe q) a

dfaStep :: (Ord c, Ord q) => c -> DfaState q c ()
dfaStep c = do
  table <- ask
  modify $ \maybeQ -> do
    q <- maybeQ
    M.lookup (q, c) table

transformToIntegerStates :: Ord q => Dfa q c -> (Dfa Int c, [(Int, q)])
transformToIntegerStates dfa =
  let mapping = M.fromList $ zip (dfaStates dfa) ([0..] :: [Int])
      replaceStates ((q, c), q') = ((mapping M.! q, c), mapping M.! q')
  in (Dfa { dfaAlphabet = dfaAlphabet dfa
          , dfaStates = M.elems mapping
          , dfaInitialState = mapping M.! dfaInitialState dfa
          , dfaFinalStates = map ((M.!) mapping) (dfaFinalStates dfa)
          , dfaTransitionFunction = map replaceStates (dfaTransitionFunction dfa)
         }, zip [0..] (dfaStates dfa))
