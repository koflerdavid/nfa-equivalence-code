module RegexDerivation where

import           Algorithm.Regex.Derivation
import           Data.Regex.Formats         ( MinimallyQuotedRegex(..) )
import           Language.RegexParser

import           Control.Monad.Trans.Class  ( lift )
import           Control.Monad.Trans.Except ( ExceptT, throwE )
import qualified Data.Text.IO               as TIO

parseAndDeriveRegexByWord :: String -> ExceptT String IO ()
parseAndDeriveRegexByWord word = do
    input <- lift TIO.getContents
    case parseRegex "<stdin>" input of
        Left parseError -> throwE parseError
        Right regex -> do
            lift . print . MinimallyQuotedRegex $ regex
            let regex' = wordDerive word regex
            lift . print . MinimallyQuotedRegex $ regex'
