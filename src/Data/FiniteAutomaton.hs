{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FiniteAutomaton
    ( FiniteAutomaton(..)
    , IsBoolean(..)
    , faAcceptingStates
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

faAcceptingStates :: (Ord q, IsBoolean o, FiniteAutomaton m q a o) => m -> Set q
faAcceptingStates fa = Data.Set.filter (isTruthy . faOutput fa) (faStates fa)

class IsBoolean b where
    isTruthy :: b -> Bool
    isTruthy = not . isFalsy

    isFalsy :: b -> Bool
    isFalsy = not . isTruthy

instance IsBoolean Bool where
    isTruthy = id
    isFalsy = not

instance IsBoolean (Maybe a) where
    isTruthy = maybe False (const True)

instance IsBoolean (Either a b) where
    isTruthy = either (const False) (const True)
