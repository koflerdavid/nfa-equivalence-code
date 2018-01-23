{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Dfa.Regex
    ( RegexDfa
    , RegexDfaTransitions
    , fromRegex
    , initialRegex
    , transitions
    ) where

import Algorithm.Regex.Derivation    ( derive )
import Algorithm.Regex.DfaConversion ( deriveRegexToDfa )
import Data.FiniteAutomaton
import Data.Regex                    ( Regex, alphabet, matchesEmptyWord )

import Data.List                     as List
import Data.Map                      as Map
import Data.Set                      as Set

type RegexDfaTransitions c = Map (Regex c) (Map c (Regex c))

data RegexDfa c = RegexDfa
    { _transitions  :: RegexDfaTransitions c
    , _inputSymbols :: Set c
    , _initialRegex :: Regex c
    }

initialRegex :: RegexDfa c -> Regex c
initialRegex = _initialRegex

-- Hack to keep the record from being modified
transitions :: RegexDfa c -> RegexDfaTransitions c
transitions = _transitions

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
