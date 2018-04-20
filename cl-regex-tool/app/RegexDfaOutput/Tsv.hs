module RegexDfaOutput.Tsv
    ( printTransitionTable
    ) where

import Algorithm.Regex.DfaConversion ( fromRegex, regexDfaTransitions )
import Data.Regex                    ( Regex, alphabet, matchesEmptyWord )
import Data.Regex.Formats            ( toMinimallyQuotedString )

import Control.Monad                 ( forM_ )
import Data.Map                      as Map
import Data.Set                      as Set

printTransitionTable :: Bool -> Regex Char -> IO ()
printTransitionTable _ regex = do
    forM_ (Set.toAscList $ alphabet regex) $ \character -> do
        putChar '\t'
        putStr (show character)
    putChar '\n'
    forM_ (Map.toList . regexDfaTransitions . fromRegex $ regex) $ \(r, ts) -> do
        putStr $
            if regex == r
                then "-> "
                else "   "
        putStr $
            if matchesEmptyWord r
                then " * "
                else " "
        putStr (toMinimallyQuotedString r)
        forM_ (Map.elems ts) $ \destination -> do
            putChar '\t'
            putStr (toMinimallyQuotedString destination)
        putChar '\n'
