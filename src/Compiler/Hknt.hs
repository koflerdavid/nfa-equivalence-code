module Compiler.Hknt where


import Data.List
import qualified Data.Map as M

import Data.Dfa


type Transition s c = (s, c, s)

compileHkntToDfa :: (Ord s, Ord c) => [Transition s c] -> [s] -> Either String (Dfa c, M.Map s Int)
compileHkntToDfa transitions acceptingStates = do
  let
    transitionStates = nub $ concatMap (\(s, _, d) -> [s, d]) transitions
    stateNumbers = M.fromList $ zip transitionStates [0..]
    acceptingStatesNotInMap = filter (`M.notMember` stateNumbers) (nub acceptingStates)
    additionalStateNumbers = M.fromList $ zip acceptingStatesNotInMap [M.size stateNumbers..]
    stateNumbers' = stateNumbers `M.union` additionalStateNumbers
  transitions' <- maybe (Left "Error translating transitions") return $ do
    mapM (translate stateNumbers') transitions
  initialState <- maybe (Left "Error finding initial state") return $ do
    if null transitionStates
      then return (M.size stateNumbers')
      else head transitionStates `M.lookup` stateNumbers'
  acceptingStates' <- maybe (Left "Error translating accepting states") return $ do
    mapM (`M.lookup` stateNumbers') (nub acceptingStates)
  return (buildDfa initialState acceptingStates' transitions', stateNumbers')


-- | This function looks up the origin and destination states and makes the transition suitable for the DFA builder.
translate :: Ord s => M.Map s Int -> Transition s c -> Maybe ((Int, c), Int)
translate mapping (origin, c, destination) =
  (,) <$> ((,) <$> origin `M.lookup` mapping <*> pure c) <*> destination `M.lookup` mapping
