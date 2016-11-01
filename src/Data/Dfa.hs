module Data.Dfa
  (
    Dfa(..)
  , accepts
  , buildDfa
  , runDfa
  , dfaAlphabet
  , dfaStep
  ) where

import Control.Monad
import Control.Monad.Trans.RWS.Strict as RWS
import Data.IntSet as IS
import Data.Map as M
import Data.Set as Set

data Dfa c =
  Dfa { dfaStates :: IntSet
      , dfaInitialState :: Int
      , dfaFinalStates :: IntSet
      , dfaErrorState :: Int
      , dfaTransitionFunction :: Map (Int, c) Int
      }
      deriving (Eq, Show)

buildDfa :: Ord c => Int -> [Int] -> [((Int, c), Int)] -> Dfa c
buildDfa initialState finalStates transitions =
  let statesFromTransitions = IS.unions $ Prelude.map (\((q, _c), q') -> IS.fromList [q, q']) transitions
      states = initialState `IS.insert` statesFromTransitions `IS.union` IS.fromList finalStates
      errorState = succ (IS.findMax states)
      states' = errorState `IS.insert` states
  in Dfa states' initialState (IS.fromList finalStates) errorState (M.fromList transitions)

runDfa :: Ord c => Dfa c -> [c] -> Int
runDfa dfa input =
  let transitionTable = dfaTransitionFunction dfa
  in case execRWS (forM_ input step) transitionTable (Just (dfaInitialState dfa)) of
    (Nothing, _) -> dfaErrorState dfa
    (Just q, _) -> q

accepts :: Dfa c -> Int -> Bool
accepts dfa q = q `IS.member` dfaFinalStates dfa

type Transitions c = (M.Map (Int, c) Int)
type DfaExecutionState c a = RWS (Transitions c) () (Maybe Int) a

step :: Ord c => c -> DfaExecutionState c ()
step c = do
  table <- ask
  RWS.modify $ \maybeQ -> do
    q <- maybeQ
    M.lookup (q, c) table

-- | Computes the alphabet of the DFA.
-- This is the set of all characters for which any transition is defined.
dfaAlphabet :: (Ord c) => Dfa c -> Set c
dfaAlphabet = Set.map snd . keysSet . dfaTransitionFunction

dfaStep :: (Ord c) => Dfa c -> (Int, c) -> Int
dfaStep dfa state_and_input =
    M.findWithDefault (dfaErrorState dfa) state_and_input (dfaTransitionFunction dfa)
