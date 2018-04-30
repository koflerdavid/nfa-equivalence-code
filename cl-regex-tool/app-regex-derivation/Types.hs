{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Exception.Safe ( Exception(displayException) )
import Options.Generic

data OutputFormat
    = Latex
    | Tsv
    | Html
    deriving (Eq, Generic, ParseField, ParseFields, Read, Show)

instance ParseRecord OutputFormat

data RegexParseException = RegexParseException String
    deriving (Show)

instance Exception RegexParseException where
    displayException (RegexParseException parseError) = parseError
