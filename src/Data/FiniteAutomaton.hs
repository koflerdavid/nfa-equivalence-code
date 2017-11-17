{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FiniteAutomaton
    ( FiniteAutomaton(..)
    , Data.Set.Set
    , Data.Map.Map
    ) where

import Data.Map
import Data.Set

-- | This is a coinductive representation of a finite automaton.
-- A finite automaton has a finite amount of states and inputs.
-- This allows for the usage of sets and maps instead of general functions that
-- might not have a compact representation.
class FiniteAutomaton m q a o | m -> q, m -> a, m -> o where
    faStates :: m -> Set q
    faInputs :: m -> Set a
    faOutput :: m -> q -> o
    faTransitions :: m -> q -> Map a (Set q)
