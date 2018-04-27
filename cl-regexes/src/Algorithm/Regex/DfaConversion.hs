{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Algorithm.Regex.DfaConversion
    ( deriveRegexToDfa
    , RegexDfa
    , RegexDfaTransitions
    , regexDfaInitialState
    , regexDfaTransitions
    , fromRegex
    , toDfa
    ) where

import           Algorithm.Regex.Derivation             ( derive )
import           Algorithm.Regex.DfaConversion.Internal
import           Data.Dfa                               ( Dfa, DfaState,
                                                          dfaErrorState )
import           Data.Dfa.Builder                       ( emptyDfa )
import           Data.FiniteAutomaton
import           Data.Queue                             as Queue
import           Data.Regex                             ( Regex (Empty),
                                                          alphabet,
                                                          matchesEmptyWord )

import           Control.Monad.Trans.State              ( State, evalState,
                                                          execState, gets,
                                                          modify )
import           Data.Bifunctor                         ( second )
import           Data.Bimap                             ( Bimap )
import qualified Data.Bimap                             as Bimap
import           Data.List                              as List
import qualified Data.Map                               as Map
import           Data.Set                               ( Set )
import qualified Data.Set                               as Set

deriveRegexToDfa :: Ord c => Regex c -> RegexDfaTransitions c
deriveRegexToDfa regex =
    let queue = FifoQueue [] `Queue.push` regex
    in evalState processQueue (Map.empty, queue)
  where
    regexAlphabet = Set.toAscList (alphabet regex)
    processQueue = do
        queue <- gets snd
        case Queue.pop queue of
            Nothing -> gets fst -- return the transition table
            Just (r, queue') -> do
                modify (second (const queue'))
                deriveRegexForAlphabet regexAlphabet r
                processQueue

-- toTransitionTable :: Ord c => Dfa c -> Bimap DfaState (Regex c ) -> Either DfaState (RegexDfaTransitions c)
-- toTransitionTable dfa mapping = Map.foldrWithKey insertTransition (Right Map.empty) (dfaTransitions dfa)
--     where
--         insertTransition (from, c) to previous = do
--             rFrom <- maybe (Left from) Right (Bimap.lookup mapping from)
--             rTo <- maybe (Left to) Right (Bimap.lookup mapping to)
--             Map.insertWith (Map.union) from (Map.singleton c to) <$> previous

-- | For this to work, the alphabet must be ascending!
-- Also, all elements in the queue are assumed to not be already contained in the transition map
deriveRegexForAlphabet ::
       (Ord c, Queue q) => [c] -> Regex c -> State (RegexDfaTransitions c, q (Regex c)) ()
deriveRegexForAlphabet regexAlphabet regex =
    modify $ \(transitions, queue) ->
        let derivations = List.map (\c -> (c, derive c regex)) regexAlphabet
            regexesToQueue =
                List.filter (`Map.notMember` transitions) $
                List.map snd derivations
            transitions' =
                Map.insert regex (Map.fromAscList derivations) transitions
        in (transitions', Queue.pushAll queue regexesToQueue)

data RegexDfa c = RegexDfa
    { rdTransitions  :: RegexDfaTransitions c
    , rdInputSymbols :: Set c
    , rdInitialRegex :: Regex c
    } deriving (Show)

regexDfaInitialState :: RegexDfa c -> Regex c
regexDfaInitialState = rdInitialRegex

-- Hack to keep the record from being modified
regexDfaTransitions :: RegexDfa c -> RegexDfaTransitions c
regexDfaTransitions = rdTransitions

instance Ord c => FiniteAutomaton (RegexDfa c) (Regex c) c Bool where
    faStates regexDfa = Map.keysSet (rdTransitions regexDfa) `Set.union` destinationRegexes
      where
        destinationRegexes =
            Set.unions . -- Unify all sets of destination regexes
            List.map (Set.fromList . Map.elems) . -- Get destination regexes
            Map.elems . -- Get transition mapping for each regex
            rdTransitions $ -- Convert regex to transition system using derivatives
            regexDfa
    faInputs = rdInputSymbols
    faOutput _ = matchesEmptyWord
    faTransitions regexDfa r =
        case r `Map.lookup` rdTransitions regexDfa of
            Just ts -> Map.map Set.singleton ts
            Nothing -> Map.fromSet (\c -> Set.singleton $ derive c r) (rdInputSymbols regexDfa)

fromRegex :: Ord c => Regex c -> RegexDfa c
fromRegex regex =
    RegexDfa
    { rdTransitions = deriveRegexToDfa regex
    , rdInputSymbols = alphabet regex
    , rdInitialRegex = regex
    }

toDfa ::
       forall c. Ord c
    => Regex c
    -> (Dfa c, Bimap (Regex c) DfaState)
toDfa (fromRegex -> regexDfa) =
    let (_n, dfa, mapping) = execState doIt (1, emptyDfa, Map.empty)
        mapping' =
            Bimap.insert Empty dfaErrorState . Bimap.fromList . Map.toAscList . Map.map snd $
            mapping
    in (dfa, mapping')
  where
    doIt = do
        convertStateTransitions (rdTransitions regexDfa)
        markAsAcceptingStates (faAcceptingStates regexDfa)
        ensureInitialStateIsPresent (regexDfaInitialState regexDfa)
