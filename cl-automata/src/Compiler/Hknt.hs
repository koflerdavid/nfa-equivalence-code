module Compiler.Hknt (
    HkntCompileError
    , compileHkntToDfa
    , compileHkntToNfa
) where

import Data.Dfa
import Data.Nfa

import Data.Bimap          as Bimap
import Data.List           as List
import Data.Maybe          ( fromJust )

type Transition q c = (q, c, q)

data HkntCompileError = CouldNotTranslateTransitions
    | CouldNotTranslateAcceptingStates
    | FoundOverlappingTransitions
    deriving (Show)

compileHkntToDfa ::
       (Ord q, Ord c)
    => [Transition q c]
    -> [q]
    -> Either HkntCompileError (Dfa c, Bimap.Bimap q DfaState)
compileHkntToDfa transitions acceptingStates = do
    let transitionStates = nub $ concatMap (\(s, _, d) -> [s, d]) transitions
        transitionStatesMapping = Bimap.fromList $ zip transitionStates [0 ..]
        unmappedAcceptingStates =
            List.filter
                (`Bimap.notMember` transitionStatesMapping)
                (nub acceptingStates)
        unmappedAcceptingStatesMapping =
            Bimap.fromList $
            zip unmappedAcceptingStates [Bimap.size transitionStatesMapping ..]
        stateNumberMapping =
            transitionStatesMapping `bimapUnion` unmappedAcceptingStatesMapping
    transitions' <- eitherFromMaybe CouldNotTranslateTransitions $
        mapM (translate stateNumberMapping) transitions
    acceptingStates' <- eitherFromMaybe CouldNotTranslateAcceptingStates $
        mapM (`Bimap.lookup` stateNumberMapping) (nub acceptingStates)
    dfa <- eitherFromMaybe FoundOverlappingTransitions $
        buildDfa acceptingStates' transitions'
    -- Should be safe since both `statesMapping` and `dfa`
    -- have been created by this function
    let statesMapping' = Bimap.mapMonotonicR (fromJust . toDfaState dfa) stateNumberMapping
    return (dfa, statesMapping')

-- | This function looks up the origin and destination states and makes the transition suitable for the DFA builder.
translate :: Ord s => Bimap s Int -> Transition s c -> Maybe ((Int, c), Int)
translate mapping (origin, c, destination) =
    (,) <$> ((,) <$> origin `Bimap.lookup` mapping <*> pure c) <*>
    destination `Bimap.lookup` mapping

-- | Run the supplied action in the Either monad. If it fails, fail with the provided error value.
eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe l = maybe (Left l) Right

compileHkntToNfa ::
       (Ord q, Ord c)
    => [Transition q c]
    -> [q]
    -> Either HkntCompileError (Nfa c, Bimap q Int)
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
        eitherFromMaybe CouldNotTranslateTransitions $
            mapM (translateNfaStates stateNumberMapping) transitions
    acceptingStates' <-
        eitherFromMaybe CouldNotTranslateAcceptingStates $
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
