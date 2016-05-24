module Data.Dfa
  (
    Dfa(..),
    accepted,
    buildDfa,
    runDfa,
    transformToIntegerStates
  ) where

import Control.Monad
import Control.Monad.Trans.RWS.Strict as RWS
import Control.Monad.Trans.State.Lazy as State
import Data.Map as M
import Data.Set as S

data Dfa q c =
  Dfa { dfaAlphabet :: Set c
      , dfaStates :: Set q
      , dfaInitialState :: q
      , dfaFinalStates :: Set q
      , dfaTransitionFunction :: Map (q, c) q
      }
      deriving (Eq, Show)

buildDfa :: (Ord c, Ord q) => q -> [q] -> q -> [((q, c), q)] -> Dfa q c
buildDfa initialState finalStates errorState transitions =
  let statesFromTransitions = S.unions $ Prelude.map (\((q, _c), q') -> S.fromList [q, q']) transitions
      otherStates = S.fromList $ initialState : errorState : finalStates
      states = statesFromTransitions `S.union` otherStates
      alphabet = S.fromList $ Prelude.map (\((_q, c), _q') -> c) transitions
      transitions' = complete errorState alphabet states (M.fromList transitions)
  in Dfa alphabet states initialState (S.fromList finalStates) transitions'

complete :: (Ord c, Ord q) => q -> Set c -> Set q -> Map (q, c) q -> Map (q, c) q
complete errorState alphabet states transitionTable =
  let defaultTransitions = [((q, c), errorState) | q <- S.elems states, c <- S.elems alphabet]
  in M.union transitionTable (M.fromList defaultTransitions)

runDfa :: (Ord c, Ord q) => Dfa q c -> [c] -> Either String q
runDfa dfa input =
  let transitionTable = dfaTransitionFunction dfa
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
  RWS.modify $ \maybeQ -> do
    q <- maybeQ
    M.lookup (q, c) table

transformToIntegerStates :: (Ord q, Ord c) => Dfa q c -> (Dfa Int c, [(Int, q)])
transformToIntegerStates = (`evalState` 0)  . transformToIntegerStatesS

transformToIntegerStatesS :: (Ord q, Ord c) => Dfa q c -> State Int (Dfa Int c, [(Int, q)])
transformToIntegerStatesS dfa = do
  -- Get a fresh variable
  firstVariable <- State.get

  -- Choose numbers for the states
  let mapping = M.fromList $ zip (S.toList $ dfaStates dfa) ([firstVariable..] :: [Int])
  -- consume the variables
  State.put (firstVariable + M.size mapping)

      -- First, transform the states in the keys. Then, the destination states
  let transitions = M.map ((M.!) mapping) $ M.mapKeys (\(q, c) -> (mapping M.! q, c)) $ dfaTransitionFunction dfa
      --newMappings = M.elems $ M.mapWithKey (curry replaceStates) (dfaTransitionFunction dfa)
  return (Dfa { dfaAlphabet = dfaAlphabet dfa
          , dfaStates = S.fromList $ M.elems mapping
          , dfaInitialState = mapping M.! dfaInitialState dfa
          , dfaFinalStates = S.map ((M.!) mapping) (dfaFinalStates dfa)
          -- Transform the transitions and combine them to a new Map
          , dfaTransitionFunction = transitions
         }, Prelude.map (\(a,b) -> (b, a)) (M.toList mapping))
