{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types where

import Data.Map
import Options.Generic

data OutputFormat = Latex | Tsv | Html
    deriving (Eq, Generic, ParseField, ParseFields, Read, Show)

instance ParseRecord OutputFormat
