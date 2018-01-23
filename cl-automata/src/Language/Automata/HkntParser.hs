module Language.Automata.HkntParser
    ( parseHknt
    , Check
    , Result(..)
    , Operation(..)
    ) where

import           Language.Automata.HkntParser.Class
import           Language.Automata.HkntParser.Internal
import           Language.Automata.HkntParser.Tokeniser

import           Data.Bifunctor                         ( first )
import qualified Data.Text                              as T
import           Text.Parsec

parseHknt :: T.Text -> Either String Result
parseHknt = first show . tokeniseAndParse hkntParser "<input>"

-- The format of the scanner is as described on https://perso.ens-lyon.fr/damien.pous/hknt/
-- The only whitespace restriction enforced is that
--  (i) the declarations have to be separated by newlines
--  (ii) within the arrows there is no freedom for whitespaces
hkntParser :: Parsec [(SourcePos, Token)] () Result
hkntParser = Result <$> transitions <*> acceptingStates <*> checks
