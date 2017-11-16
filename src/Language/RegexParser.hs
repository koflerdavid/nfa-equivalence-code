module Language.RegexParser ( parseRegex ) where

import Data.Regex
import Language.RegexParser.Internal

import Text.Parsec                   ( SourceName )

parseRegex :: SourceName -> String -> Either String (Regex Char)
parseRegex = tokeniseAndParse regex
