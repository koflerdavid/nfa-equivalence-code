{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types where

import           Data.Regex

import           Data.Map
import           Options.Generic

type RegexDfaTransitions = Map (Regex Char) (Map Char (Regex Char))

data OutputFormat = Latex | Tsv | Html
    deriving (Eq, Generic, ParseField, ParseFields, Read, Show)

instance ParseRecord OutputFormat
