module RegexDfaOutput.Tsv ( printTransitionTable ) where

import Data.Dfa.Regex ( RegexDfaTransitions )
import Data.Regex     ( Regex, alphabet, matchesEmptyWord )

import Control.Monad  ( forM_ )
import Data.Map       as Map
import Data.Set       as Set

printTransitionTable :: Bool -> Regex Char -> RegexDfaTransitions -> IO ()
printTransitionTable _ regex transitions = do
    forM_ (Set.toAscList $ alphabet regex) $ \c -> putStr ('\t' : show c)
    putStr "\n"
    forM_ (Map.toList transitions) $
        \(r, ts) -> do
            putStr $ if regex == r then "-> " else "   "
            putStr $ if matchesEmptyWord r then " * " else " "
            putStr (show r)
            forM_ (Map.elems ts) $ \r' -> putStr ('\t' : show r')
            putStr "\n"
