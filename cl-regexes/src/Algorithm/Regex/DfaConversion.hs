{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.Regex.DfaConversion
    ( deriveRegexToDfa
    , RegexDfa
    , RegexDfaTransitions
    , regexDfaInitialState
    , regexDfaTransitions
    , fromRegex
    , toDfa
    ) where

import Algorithm.Regex.Derivation ( derive )
import Data.Dfa
import Data.FiniteAutomaton
import Data.Queue                 as Queue
import Data.Regex                 ( Regex, alphabet, matchesEmptyWord  )

import Control.Monad.Trans.State  ( State, evalState, gets, modify )
import Data.Bifunctor             ( second )
import Data.Bimap                 as Bimap
import Data.List                  as List
import Data.Map                   as Map
import Data.Maybe                 ( fromJust )
import Data.Set                   as Set

type RegexDfaTransitions c = Map (Regex c) (Map c (Regex c))

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

-- | For this to work, the alphabet must be ascending!
-- Also, all elements in the queue are assumed to not be already contained in the transition map
deriveRegexForAlphabet ::
       (Ord c, Queue q)
    => [c]
    -> Regex c
    -> State (RegexDfaTransitions c, q (Regex c)) ()
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
    { _transitions  :: RegexDfaTransitions c
    , _inputSymbols :: Set c
    , _initialRegex :: Regex c
    }

regexDfaInitialState :: RegexDfa c -> Regex c
regexDfaInitialState = _initialRegex

-- Hack to keep the record from being modified
regexDfaTransitions :: RegexDfa c -> RegexDfaTransitions c
regexDfaTransitions = _transitions

instance Ord c => FiniteAutomaton (RegexDfa c) (Regex c) c Bool where
    faStates regexDfa =
        Map.keysSet (_transitions regexDfa) `Set.union` destinationRegexes
      where
        destinationRegexes =
            Set.unions . -- Unify all sets of destination regexes
            List.map (Set.fromList . Map.elems) . -- Get destination regexes
            Map.elems . -- Get transition mapping for each regex
            _transitions $ -- Convert regex to transition system using derivatives
            regexDfa
    faInputs = _inputSymbols
    faOutput _ = matchesEmptyWord
    faTransitions regexDfa r =
        case r `Map.lookup` _transitions regexDfa of
            Just ts -> Map.map Set.singleton ts
            Nothing -> Map.fromSet (\c -> Set.singleton $ derive c r) (_inputSymbols regexDfa)

fromRegex :: Ord c => Regex c -> RegexDfa c
fromRegex regex = RegexDfa {
            _transitions = deriveRegexToDfa regex
         , _inputSymbols = alphabet regex
         , _initialRegex = regex
         }

toDfa :: Ord c => Regex c -> (Dfa c, Bimap (Regex c) DfaState)
toDfa regex =
        (dfa, regexesToDfaStates)
    where
        dfa = buildDfaUnsafe (Set.toList convertedAcceptingStates) convertedTransitions
        regexesToDfaStates = Bimap.mapR (fromJust . toDfaState dfa) regexesToIntegers

        regexDfa = fromRegex regex
        regexes = faStates regexDfa
        regexesToIntegers = Bimap.fromList $ [(s, i) | s <- Set.toList regexes, i <- [0..]]
        acceptingStates = Set.filter ((== True ) . faOutput regexDfa) regexes
        convertedAcceptingStates = Set.map ((Bimap.!) regexesToIntegers) $ acceptingStates
        transitionMap = regexDfaTransitions regexDfa
        convertedTransitions =
            (`concatMap` Map.toList transitionMap) $ \(originRegex, inputToDestination) ->
                let origin = regexesToIntegers Bimap.! originRegex
                in (`List.map` Map.toList inputToDestination) $
                    \ (c, destinationRegex) ->
                        let destination = regexesToIntegers Bimap.! destinationRegex
                        in ((origin, c), destination)
