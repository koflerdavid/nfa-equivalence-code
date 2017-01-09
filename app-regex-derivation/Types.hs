module Types where

import           Data.Regex

import           Data.Map

type RegexDfaTransitions = Map (Regex Char) (Map Char (Regex Char))
