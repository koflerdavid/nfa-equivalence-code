module RegexDerivation where

import Algorithm.Regex.Derivation
import Data.Regex
import Language.RegexParser

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except

parseAndDeriveRegexByWord :: String -> ExceptT String IO ()
parseAndDeriveRegexByWord word = do
    input <- lift $ getContents
    case parseRegex "<stdin>" input of
        Left parseError -> throwE parseError
        Right regex -> do
            lift $ putStrLn (show regex)
            let regex' = wordDerive word regex
            lift $ putStrLn (show regex')
