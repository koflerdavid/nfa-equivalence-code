module Algorithm.AutomataMergeSpec (main, spec) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)

import Test.Hspec

import Algorithm.AutomataMerge
import Data.Dfa

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    do describe "union" $
           do forM_ samples $ \(i, (dfa1, dfa2, merged)) ->
                  do it ("should work for sample #" ++ show i) $
                         do snd (dfa1 `mergeDfa` dfa2) `shouldBe` merged

samples :: [(Int, (Dfa Char, Dfa Char, Dfa Char))]
samples = [1..] `zip` [sample1]

sample1 :: (Dfa Char, Dfa Char, Dfa Char)
sample1 = (
    fromJust $ buildDfa [1] [((0, 'a'), 1)],
    fromJust $ buildDfa [0] [((0, 'a'), 0), ((0, 'b'), 1)],
    fromJust $ buildDfa [1, 2] [((0, 'a'), 1), ((2, 'a'), 2), ((2, 'b'), 3)]
    )
