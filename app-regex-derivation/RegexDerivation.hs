module RegexDerivation where

import Algorithm.Regex.Derivation
import Language.RegexParser
import Data.Regex.Formats         ( FullyQuotedRegex(..) )

import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Except ( ExceptT, throwE )

parseAndDeriveRegexByWord :: String -> ExceptT String IO ()
parseAndDeriveRegexByWord word = do
    input <- lift getContents
    case parseRegex "<stdin>" input of
        Left parseError -> throwE parseError
        Right regex -> do
            lift $ print (FullyQuotedRegex regex)
            let regex' = wordDerive word regex
            lift . print . FullyQuotedRegex $ regex'
