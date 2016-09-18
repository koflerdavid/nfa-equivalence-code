module Data.Dfa
  (
    Dfa(..),
    accepts,
    buildDfa,
    runDfa,
  ) where

import Control.Monad
import Control.Monad.Trans.RWS.Strict as RWS
import Data.IntSet as IS
import Data.Map as M
import Data.Set as S

data Dfa c =
  Dfa { dfaAlphabet :: Set c
      , dfaStates :: IntSet
      , dfaInitialState :: Int
      , dfaFinalStates :: IntSet
      , dfaErrorState :: Int
      , dfaTransitionFunction :: Map (Int, c) Int
      }
      deriving (Eq, Show)

buildDfa :: Ord c => Int -> [Int] -> [((Int, c), Int)] -> Dfa c
buildDfa initialState finalStates transitions =
  let alphabet = S.fromList $ Prelude.map (\((_q, c), _q') -> c) transitions
      statesFromTransitions = IS.unions $ Prelude.map (\((q, _c), q') -> IS.fromList [q, q']) transitions
      states = initialState `IS.insert` statesFromTransitions `IS.union` IS.fromList finalStates
      errorState = succ (IS.findMax states)
  in Dfa alphabet states initialState (IS.fromList finalStates) errorState (M.fromList transitions)

runDfa :: Ord c => Dfa c -> [c] -> Int
runDfa dfa input =
  let transitionTable = dfaTransitionFunction dfa
  in case execRWS (forM_ input dfaStep) transitionTable (Just (dfaInitialState dfa)) of
    (Nothing, _) -> dfaErrorState dfa
    (Just q, _) -> q

accepts :: Dfa c -> Int -> Bool
accepts dfa q = q `IS.member` dfaFinalStates dfa

type Transitions c = (M.Map (Int, c) Int)
type DfaState c a = RWS (Transitions c) () (Maybe Int) a

dfaStep :: Ord c => c -> DfaState c ()
dfaStep c = do
  table <- ask
  RWS.modify $ \maybeQ -> do
    q <- maybeQ
    M.lookup (q, c) table
