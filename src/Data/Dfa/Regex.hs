module Data.Dfa.Regex ( RegexDfaTransitions ) where

import Data.Regex ( Regex )

import Data.Map

type RegexDfaTransitions = Map (Regex Char) (Map Char (Regex Char))
