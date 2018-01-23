module RegexDfaOutput.Tsv
    ( printTransitionTable
    ) where

import Data.Dfa.Regex     ( fromRegex, transitions )
import Data.Regex         ( Regex, alphabet, matchesEmptyWord )
import Data.Regex.Formats ( MinimallyQuotedRegex (..) )

import Control.Monad      ( forM_ )
import Data.Map           as Map
import Data.Set           as Set

printTransitionTable :: Bool -> Regex Char -> IO ()
printTransitionTable _ regex = do
    forM_ (Set.toAscList $ alphabet regex) $ putStr . ('\t' :) . show
    putChar '\n'
    forM_ (Map.toList . transitions . fromRegex $ regex) $ \(r, ts) -> do
        putStr $
            if regex == r
                then "-> "
                else "   "
        putStr $
            if matchesEmptyWord r
                then " * "
                else " "
        putStr . show . MinimallyQuotedRegex $ r
        forM_ (Map.elems ts) $ putStr . ('\t' :) . show . MinimallyQuotedRegex
        putChar '\n'
