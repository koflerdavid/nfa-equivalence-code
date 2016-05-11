module Data.Dfa
  (
    Dfa(..),
    accepted,
    complete,
    runDfa,
    transformToIntegerStates
  ) where

import Control.Monad
import Control.Monad.Trans.RWS.Strict
import qualified Data.Map.Strict as M

data Dfa q c =
  Dfa { dfaAlphabet :: [c]
      , dfaStates :: [q]
      , dfaInitialState :: q
      , dfaFinalStates :: [q]
      , dfaTransitionFunction :: [((q, c), q)]
      }
      deriving (Eq, Show)

complete :: (Ord c, Ord q) => q -> Dfa q c -> Dfa q c
complete defaultState dfa =
  let transitionTable = M.fromList (dfaTransitionFunction dfa)
      defaultTransitions = [((q, c), defaultState) | q <- dfaStates dfa, c <- dfaAlphabet dfa]
      table = M.union transitionTable (M.fromList defaultTransitions)
  in dfa { dfaTransitionFunction = M.toList table }

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
