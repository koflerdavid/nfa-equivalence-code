module AutomataOutput where

import           Data.Nfa

import           Data.GraphViz
import           Data.GraphViz.Printing ( renderDot, toDot )
import           Data.Map               as Map
import           Data.IntSet            as IntSet
import           Data.Text.Lazy

renderAutomata :: Nfa Char -> Text
renderAutomata nfa = renderDot
    . toDot
    $ graphElemsToDot params nodes edges
  where
    params :: GraphvizParams Int Int String () Int
    params = defaultParams
    nodes :: [(Int, Int)]
    nodes = Prelude.zip [0 .. nodeCount] [0 .. nodeCount]
    nodeCount :: Int
    nodeCount = Prelude.foldr max 0 $ Prelude.map (\(p, q, _) -> max p q) $ edges
    edges :: [(Int, Int, String)]
    edges = Prelude.concatMap (\((p, c), qs) -> Prelude.map (\q -> (p, q, show c)) (IntSet.toList qs))
        . Map.toList
        $ nfaTransitions nfa
