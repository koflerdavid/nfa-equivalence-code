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
import Data.IntSet as IS
import Data.Map as M
import Data.Set as S

data Dfa c =
  Dfa { dfaAlphabet :: Set c
      , dfaStates :: IntSet
      , dfaInitialState :: Int
      , dfaFinalStates :: IntSet
      , dfaTransitionFunction :: Map (Int, c) Int
      }
      deriving (Eq, Show)

buildDfa :: Ord c => Int -> [Int] -> Int -> [((Int, c), Int)] -> Dfa c
buildDfa initialState finalStates errorState transitions =
  let statesFromTransitions = IS.unions $ Prelude.map (\((q, _c), q') -> IS.fromList [q, q']) transitions
      otherStates = IS.fromList $ initialState : errorState : finalStates
      states = statesFromTransitions `IS.union` otherStates
      alphabet = S.fromList $ Prelude.map (\((_q, c), _q') -> c) transitions
      transitions' = complete errorState alphabet states (M.fromList transitions)
  in Dfa alphabet states initialState (IS.fromList finalStates) transitions'

complete :: Ord c => Int -> Set c -> IntSet -> Map (Int, c) Int -> Map (Int, c) Int
complete errorState alphabet states transitionTable =
  let defaultTransitions = [((q, c), errorState) | q <- IS.elems states, c <- S.elems alphabet]
  in M.union transitionTable (M.fromList defaultTransitions)

runDfa :: Ord c => Dfa c -> [c] -> Either String Int
runDfa dfa input =
  let transitionTable = dfaTransitionFunction dfa
  in case execRWS (forM_ input dfaStep) transitionTable (Just (dfaInitialState dfa)) of
    (Nothing, _) -> Left "Encountered an undefined state transition"
    (Just q, _) -> Right q

accepted :: Dfa c -> Int -> Bool
accepted dfa q = q `IS.member` dfaFinalStates dfa

type Transitions c = (M.Map (Int, c) Int)
type DfaState c a = RWS (Transitions c) () (Maybe Int) a

dfaStep :: Ord c => c -> DfaState c ()
dfaStep c = do
  table <- ask
  RWS.modify $ \maybeQ -> do
    q <- maybeQ
    M.lookup (q, c) table

transformToIntegerStates :: Ord c => Dfa c -> (Dfa c, [(Int, Int)])
transformToIntegerStates = (`evalState` 0)  . transformToIntegerStatesS

transformToIntegerStatesS :: Ord c => Dfa c -> State Int (Dfa c, [(Int, Int)])
transformToIntegerStatesS dfa = do
  -- Get a fresh variable
  firstVariable <- State.get

  -- Choose numbers for the states
  let mapping = M.fromList $ zip (IS.toList $ dfaStates dfa) ([firstVariable..] :: [Int])
  -- consume the variables
  State.put (firstVariable + M.size mapping)

      -- First, transform the states in the keys. Then, the destination states
  let transitions = M.map ((M.!) mapping) $ M.mapKeys (\(q, c) -> (mapping M.! q, c)) $ dfaTransitionFunction dfa
      --newMappings = M.elems $ M.mapWithKey (curry replaceStates) (dfaTransitionFunction dfa)
  return (Dfa { dfaAlphabet = dfaAlphabet dfa
          , dfaStates = IS.fromList $ M.elems mapping
          , dfaInitialState = mapping M.! dfaInitialState dfa
          , dfaFinalStates = IS.map ((M.!) mapping) (dfaFinalStates dfa)
          -- Transform the transitions and combine them to a new Map
          , dfaTransitionFunction = transitions
         }, Prelude.map (\(a,b) -> (b, a)) (M.toList mapping))
