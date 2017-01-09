module RegexDfaOutput.Tsv ( printTransitionTable ) where

import           Types

import           Data.Regex

import           Control.Monad ( forM_ )
import           Data.Map      as Map
import           Data.Set      as Set

printTransitionTable :: Bool -> Regex Char -> RegexDfaTransitions -> IO ()
printTransitionTable _ regex transitions = do
    forM_ (Set.toAscList $ alphabet regex) $ \c -> putStr ('\t' : show c)
    putStr "\n"
    forM_ (Map.toList transitions) $
        \(r, ts) -> do
            putStr $ if regex == r then "-> " else "   "
            putStr $ if matchesEmptyWord r then " * " else " "
            putStr (show r)
            forM_ (Map.toAscList ts) $
                \(c, r') -> do
                    putStr ('\t' : show r')
            putStr "\n"
