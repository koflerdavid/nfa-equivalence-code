module RegexDerivation where

import           Algorithm.Regex.Derivation
import           Data.Regex.Formats         ( toMinimallyQuotedString )
import           Language.RegexParser
import           Types

import           Control.Exception.Safe     ( throw )
import qualified Data.Text.IO               as TIO

parseAndDeriveRegexByWord :: String -> IO ()
parseAndDeriveRegexByWord word = do
    input <- TIO.getContents
    case parseRegex "<stdin>" input of
        Left parseError -> throw (RegexParseException parseError)
        Right regex -> do
            putStrLn (toMinimallyQuotedString regex)
            let regex' = wordDerive word regex
            putStrLn (toMinimallyQuotedString regex')
