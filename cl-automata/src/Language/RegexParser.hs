module Language.RegexParser
    ( parseRegex
    ) where

import           Data.Regex
import           Language.RegexParser.Internal

import qualified Data.Text                      as T
import           Text.Parsec                   ( SourceName )

parseRegex :: SourceName -> T.Text -> Either String (Regex Char)
parseRegex = tokeniseAndParse regex
