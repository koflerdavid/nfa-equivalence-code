module RegexDfaOutput.Tsv
    ( printTransitionTable
    ) where

import Data.Regex         ( Regex, alphabet, matchesEmptyWord )
import Data.Regex.Formats ( MinimallyQuotedRegex(..) )

import Control.Monad      ( forM_ )
import Data.Map           as Map
import Data.Set           as Set

type RegexDfaTransitions c = Map (Regex c) (Map c (Regex c))

printTransitionTable :: Bool -> Regex Char -> RegexDfaTransitions Char -> IO ()
printTransitionTable _ regex transitions = do
    forM_ (Set.toAscList $ alphabet regex) $ putStr . ('\t' :)  . show
    putStr "\n"
    forM_ (Map.toList transitions) $ \(r, ts) -> do
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
        putStr "\n"
