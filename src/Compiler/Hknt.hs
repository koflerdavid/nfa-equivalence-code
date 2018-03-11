module Compiler.Hknt (
    compileHkntToDfa
    , compileHkntToNfa
) where

import Data.Bimap as Bimap
import Data.List  as List

import Data.Dfa
import Data.Nfa

type Transition s c = (s, c, s)

compileHkntToDfa ::
       (Ord s, Ord c)
    => [Transition s c]
    -> [s]
    -> Either String (Dfa c, Bimap.Bimap s Int)
compileHkntToDfa transitions acceptingStates = do
    let transitionStates = nub $ concatMap (\(s, _, d) -> [s, d]) transitions
        transitionStatesMapping = Bimap.fromList $ zip transitionStates [0 ..]
        unmappedAcceptingStates =
            List.filter
                (`Bimap.notMember` transitionStatesMapping)
                (nub acceptingStates)
        acceptingStatesMapping =
            Bimap.fromList $
            zip unmappedAcceptingStates [Bimap.size transitionStatesMapping ..]
        stateNumberMapping =
            transitionStatesMapping `bimapUnion` acceptingStatesMapping
    transitions' <-
        eitherFromMaybe "Error translating transitions" $ do
            mapM (translate stateNumberMapping) transitions
    acceptingStates' <-
        eitherFromMaybe "Error translating accepting states" $ do
            mapM (`Bimap.lookup` stateNumberMapping) (nub acceptingStates)
    dfa <-
        eitherFromMaybe "Overlapping transitions found" $ do
            buildDfa acceptingStates' transitions'
    return (dfa, stateNumberMapping)

-- | This function looks up the origin and destination states and makes the transition suitable for the DFA builder.
translate :: Ord s => Bimap s Int -> Transition s c -> Maybe ((Int, c), Int)
translate mapping (origin, c, destination) =
    (,) <$> ((,) <$> origin `Bimap.lookup` mapping <*> pure c) <*>
    destination `Bimap.lookup` mapping

-- | Run the supplied action in the Either monad. If it fails, fail with the provided error value.
eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe l = maybe (Left l) Right

compileHkntToNfa ::
       (Ord s, Ord c)
    => [Transition s c]
    -> [s]
    -> Either String (Nfa c, Bimap s Int)
compileHkntToNfa transitions acceptingStates = do
    let transitionStates = nub $ concatMap (\(s, _, d) -> [s, d]) transitions
        transitionStatesMapping = Bimap.fromList $ zip transitionStates [0 ..]
        unmappedAcceptingStates =
            List.filter
                (`Bimap.notMember` transitionStatesMapping)
                (nub acceptingStates)
        acceptingStatesMapping =
            Bimap.fromList $
            zip unmappedAcceptingStates [Bimap.size transitionStatesMapping ..]
        stateNumberMapping =
            transitionStatesMapping `bimapUnion` acceptingStatesMapping
    transitions' <-
        eitherFromMaybe "Error translating transitions" $ do
            mapM (translateNfaStates stateNumberMapping) transitions
    acceptingStates' <-
        eitherFromMaybe "Error translating accepting states" $ do
            mapM (`Bimap.lookup` stateNumberMapping) (nub acceptingStates)
    let nfa = buildNfa acceptingStates' transitions'
    return (nfa, stateNumberMapping)

-- | This function looks up the origin and destination states and makes the transition suitable for the DFA builder.
translateNfaStates ::
       Ord s => Bimap s Int -> Transition s c -> Maybe ((Int, c), [Int])
translateNfaStates mapping (origin, c, destination) =
    (,) <$> ((,) <$> origin `Bimap.lookup` mapping <*> pure c) <*>
    fmap (: []) (destination `Bimap.lookup` mapping)

bimapUnion :: (Ord a, Ord b) => Bimap.Bimap a b -> Bimap.Bimap a b -> Bimap.Bimap a b
bimapUnion mapA mapB = foldr (\(a, b) result -> Bimap.insert a b result) mapA $ Bimap.toAscList mapB
