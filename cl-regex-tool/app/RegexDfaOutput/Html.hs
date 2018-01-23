module RegexDfaOutput.Html
    ( printTransitionTable
    ) where

import           Data.Dfa.Format.Html ( asHtml )
import           Data.Regex

import qualified Data.Text.Lazy.IO    as TIO

printTransitionTable :: Bool -> Regex Char -> IO ()
printTransitionTable withoutSkeleton regex =
    TIO.putStrLn (asHtml withoutSkeleton regex)
