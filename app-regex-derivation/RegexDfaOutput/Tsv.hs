module RegexDfaOutput.Tsv
    ( printTransitionTable
    ) where

import Data.Regex         ( Regex, alphabet, matchesEmptyWord )
import Data.Regex.Formats ( FullyQuotedRegex(..) )

import Control.Monad      ( forM_ )
import Data.Map           as Map
import Data.Set           as Set

type RegexDfaTransitions c = Map (Regex c) (Map c (Regex c))

printTransitionTable :: Bool -> Regex Char -> RegexDfaTransitions Char -> IO ()
printTransitionTable _ regex transitions = do
    forM_ (Set.toAscList $ alphabet regex) $ \c -> putStr ('\t' : show c)
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
        putStr . show . FullyQuotedRegex $ r
        forM_ (Map.elems ts) $ \r' -> putStr ('\t' : show (FullyQuotedRegex r'))
        putStr "\n"
