module Compiler.Hknt (
    compileHkntToDfa
    , compileHkntToNfa
    , invertedStateMapping
) where

import Data.List as List
import Data.Map  as Map

import Data.Dfa
import Data.Nfa

type Transition s c = (s, c, s)

compileHkntToDfa ::
       (Ord s, Ord c)
    => [Transition s c]
    -> [s]
    -> Either String (Dfa c, Map s Int)
compileHkntToDfa transitions acceptingStates = do
    let transitionStates = nub $ concatMap (\(s, _, d) -> [s, d]) transitions
        transitionStatesMapping = Map.fromList $ zip transitionStates [0 ..]
        unmappedAcceptingStates =
            List.filter
                (`Map.notMember` transitionStatesMapping)
                (nub acceptingStates)
        acceptingStatesMapping =
            Map.fromList $
            zip unmappedAcceptingStates [Map.size transitionStatesMapping ..]
        stateNumberMapping =
            transitionStatesMapping `Map.union` acceptingStatesMapping
    transitions' <-
        eitherFromMaybe "Error translating transitions" $ do
            mapM (translate stateNumberMapping) transitions
    acceptingStates' <-
        eitherFromMaybe "Error translating accepting states" $ do
            mapM (`Map.lookup` stateNumberMapping) (nub acceptingStates)
    dfa <-
        eitherFromMaybe "Overlapping transitions found" $ do
            buildDfa acceptingStates' transitions'
    return (dfa, stateNumberMapping)

-- | This function looks up the origin and destination states and makes the transition suitable for the DFA builder.
translate :: Ord s => Map s Int -> Transition s c -> Maybe ((Int, c), Int)
translate mapping (origin, c, destination) =
    (,) <$> ((,) <$> origin `Map.lookup` mapping <*> pure c) <*>
    destination `Map.lookup` mapping

-- | Run the supplied action in the Either monad. If it fails, fail with the provided error value.
eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe l = maybe (Left l) Right

compileHkntToNfa ::
       (Ord s, Ord c)
    => [Transition s c]
    -> [s]
    -> Either String (Nfa c, Map s Int)
compileHkntToNfa transitions acceptingStates = do
    let transitionStates = nub $ concatMap (\(s, _, d) -> [s, d]) transitions
        transitionStatesMapping = Map.fromList $ zip transitionStates [0 ..]
        unmappedAcceptingStates =
            List.filter
                (`Map.notMember` transitionStatesMapping)
                (nub acceptingStates)
        acceptingStatesMapping =
            Map.fromList $
            zip unmappedAcceptingStates [Map.size transitionStatesMapping ..]
        stateNumberMapping =
            transitionStatesMapping `Map.union` acceptingStatesMapping
    transitions' <-
        eitherFromMaybe "Error translating transitions" $ do
            mapM (translateNfaStates stateNumberMapping) transitions
    acceptingStates' <-
        eitherFromMaybe "Error translating accepting states" $ do
            mapM (`Map.lookup` stateNumberMapping) (nub acceptingStates)
    let nfa = buildNfa acceptingStates' transitions'
    return (nfa, stateNumberMapping)

-- | This function looks up the origin and destination states and makes the transition suitable for the DFA builder.
translateNfaStates ::
       Ord s => Map s Int -> Transition s c -> Maybe ((Int, c), [Int])
translateNfaStates mapping (origin, c, destination) =
    (,) <$> ((,) <$> origin `Map.lookup` mapping <*> pure c) <*>
    fmap (: []) (destination `Map.lookup` mapping)

invertedStateMapping :: Map s Int -> Map Int s
invertedStateMapping m = Map.fromList [(s, name) | (name, s) <- Map.toList m]
