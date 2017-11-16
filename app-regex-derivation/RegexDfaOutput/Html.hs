module RegexDfaOutput.Html ( printTransitionTable ) where

import           Data.Dfa.Format.Html (asHtml)
import           Data.Regex

import           Data.Map             (Map)
import qualified Data.Text.Lazy.IO    as TIO

type RegexDfaTransitions c = Map (Regex c) (Map c (Regex c))

printTransitionTable :: Bool -> Regex Char -> RegexDfaTransitions Char -> IO ()
printTransitionTable withoutSkeleton regex _transitions =
    TIO.putStrLn (asHtml withoutSkeleton regex)
