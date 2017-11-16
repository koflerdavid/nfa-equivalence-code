module Language.Automata.HkntParser
    ( parseHknt
    , Result(..)
    , Operation(..)
    ) where

import Language.Automata.HkntParser.Class
import Language.Automata.HkntParser.Internal
import Language.Automata.HkntParser.Tokeniser

import Data.Either.Combinators                ( mapLeft )
import Text.Parsec

parseHknt :: String -> Either String Result
parseHknt input = mapLeft show $ tokeniseAndParse hkntParser "<input>" input

-- The format of the scanner is as described on https://perso.ens-lyon.fr/damien.pous/hknt/
-- The only whitespace restriction enforced is that
--  (i) the declarations have to be separated by newlines
--  (ii) within the arrows there is no freedom for whitespaces
hkntParser :: Parsec [(SourcePos, Token)] () Result
--hkntParser = spaces *> (Result <$> transitions <*> (spaces *> acceptingStates) <*> (spaces *> checks)) <* spaces
hkntParser = Result <$> transitions <*> acceptingStates <*> checks
