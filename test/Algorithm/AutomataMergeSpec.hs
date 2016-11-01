module Algorithm.AutomataMergeSpec (main, spec) where

import Control.Monad (forM_)
import Data.IntSet as ISet
import Data.Map as Map

import Test.Hspec
import Test.QuickCheck

import Algorithm.AutomataMerge
import Data.Dfa

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    do describe "union" $
           do forM_ ( zip [1..] samples) $ \(i, (dfa1, dfa2, merged)) ->
                  do it ("should work for sample #" ++ show i) $
                         do snd (dfa1 `mergeDfa` dfa2) `shouldBe` merged

samples :: [(Dfa Char, Dfa Char, Dfa Char)]
samples = [sample1]

sample1 = (
    buildDfa 0 [1] [((0, 'a'), 1)],
    buildDfa 0 [0] [((0, 'a'), 0), ((0, 'b'), 1)],
    Dfa (ISet.fromList [0..4]) 0 (ISet.fromList [1, 3]) 2 $
        Map.fromList [((0, 'a'), 1), ((3, 'a'), 3), ((3, 'b'), 4)])
