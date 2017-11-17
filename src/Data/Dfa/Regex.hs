{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Dfa.Regex
    ( RegexDfa
    , fromRegex
    , transitions
    ) where

import Algorithm.Regex.Derivation    ( derive )
import Algorithm.Regex.DfaConversion ( deriveRegexToDfa )
import Data.FiniteAutomaton
import Data.Regex                    ( Regex, alphabet, matchesEmptyWord )

import Data.List                     as List
import Data.Map                      as Map
import Data.Set                      as Set

data RegexDfa c = RegexDfa
    { _transitions  :: Map (Regex c) (Map c (Regex c))
    , _inputSymbols :: Set c
    }

-- Hack to keep the record from being modified
transitions :: RegexDfa c -> Map (Regex c) (Map c (Regex c))
transitions = _transitions

instance Ord c => FiniteAutomaton (RegexDfa c) (Regex c) c Bool where
    faStates regexDfa =
        Map.keysSet (_transitions regexDfa) `Set.union` destinationRegexes
      where
        destinationRegexes =
            Set.unions .
            List.map (Set.fromList . Map.elems) . Map.elems . _transitions $
            regexDfa
    faInputs = _inputSymbols
    faOutput _ = matchesEmptyWord
    faTransitions regexDfa r =
        case r `Map.lookup` _transitions regexDfa of
            Just ts -> Map.map Set.singleton ts
            Nothing ->
                Map.fromSet
                    (\c -> Set.singleton $ derive c r)
                    (_inputSymbols regexDfa)

fromRegex :: Ord c => Regex c -> RegexDfa c
fromRegex regex =
    RegexDfa
    {_transitions = deriveRegexToDfa regex, _inputSymbols = alphabet regex}
