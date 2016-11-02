module Compiler.Hknt where


import Data.List
import qualified Data.Map as M

import Data.Dfa


type Transition s c = (s, c, s)

compileHkntToDfa :: (Ord s, Ord c) => [Transition s c] -> [s] -> Either String (Dfa c, M.Map s Int)
compileHkntToDfa transitions acceptingStates = do
    let
        transitionStates =
            nub $ concatMap (\(s, _, d) -> [s, d]) transitions
        transitionStatesMapping =
            M.fromList $ zip transitionStates [0..]
        unmappedAcceptingStates =
            filter (`M.notMember` transitionStatesMapping) (nub acceptingStates)
        acceptingStatesMapping =
            M.fromList $ zip unmappedAcceptingStates [M.size transitionStatesMapping..]
        stateNumberMapping =
            transitionStatesMapping `M.union` acceptingStatesMapping
    transitions' <- eitherFromMaybe "Error translating transitions" $ do
        mapM (translate stateNumberMapping) transitions
    acceptingStates' <- eitherFromMaybe "Error translating accepting states" $ do
        mapM (`M.lookup` stateNumberMapping) (nub acceptingStates)
    dfa <- eitherFromMaybe "Overlapping transitions found" $ do
        buildDfa acceptingStates' transitions'
    return (dfa, stateNumberMapping)


-- | This function looks up the origin and destination states and makes the transition suitable for the DFA builder.
translate :: Ord s => M.Map s Int -> Transition s c -> Maybe ((Int, c), Int)
translate mapping (origin, c, destination) =
  (,) <$> ((,) <$> origin `M.lookup` mapping <*> pure c) <*> destination `M.lookup` mapping

eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe l = maybe (Left l) Right
