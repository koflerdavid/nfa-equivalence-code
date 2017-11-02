module RegexDfaOutput.Html ( printTransitionTable ) where

import           Data.Dfa.Format.Html ( asHtml )
import           Data.Dfa.Regex       ( RegexDfaTransitions )

import           Data.Regex
import qualified Data.Text.Lazy.IO    as TIO

printTransitionTable :: Bool -> Regex Char -> RegexDfaTransitions -> IO ()
printTransitionTable withoutSkeleton regex transitions =
    TIO.putStrLn (asHtml withoutSkeleton regex transitions)
