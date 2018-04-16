module Data.CongruenceClosureSpec
    ( spec_congruenceClosure
    ) where

import qualified Data.CongruenceClosure as CC

import           Data.IntSet            ( fromList )
import           Test.Hspec

spec_congruenceClosure :: Spec
spec_congruenceClosure = do
    describe "empty family of sets" $ do
        let emptySetOfSets = CC.empty
        it "should consider equal sets as equal" $ do
            equal [1] [1] emptySetOfSets `shouldBe` True
        it "should not consider any sets as equal" $ do
            equal [2] [3] emptySetOfSets `shouldBe` False
    let relation1 = equate CC.empty [1] [2]
    describe "equivalence relation on sets with {1} = {2}" $ do
        it "should consider {1} and {2} as equal" $ do
            equal [1] [2] relation1 `shouldBe` True
        it "should consider {1} and {1} as equal" $ do
            equal [1] [1] relation1 `shouldBe` True
        it "should consider {1} and {1, 2} as equal" $ do
            equal [1] [1, 2] relation1 `shouldBe` True
        it "should not consider {1} and {} as equal" $ do
            equal [1] [] relation1 `shouldBe` False
        it "should not consider {1} and {3} as equal" $ do
            equal [1] [3] relation1 `shouldBe` False
        it "should not consider {1, 3} and {3} as equal" $ do
            equal [1, 3] [3] relation1 `shouldBe` False
        it "should not consider {1, 3} and {4} as equal" $ do
            equal [1, 3] [4] relation1 `shouldBe` False
    describe "equivalence relation on sets with {1} = {2} and {3, 4} = {3, 5}" $ do
        let relation2 = equate relation1 [3, 4] [3, 5]
        it "should consider {3, 4} and {3, 5} as equal" $ do
            equal [3, 4] [3, 5] relation2 `shouldBe` True
        it "should consider {1, 3, 4} and {2, 3, 5} as equal" $ do
            equal [1, 3, 4] [2, 3, 5] relation2 `shouldBe` True
        it "should not consider {2, 3, 4} and {3, 5} as equal" $ do
            equal [2, 3, 4] [3, 5] relation2 `shouldNotBe` True
    describe "a congruence closure with {3, 4} and {3, 5}" $ do
        let relation3 = equate relation1 [3, 4] [3, 5]
        it "should consider {3, 4} and {3, 5} as equal" $ do
            equal [3, 4] [3, 5] relation3 `shouldBe` True
        let relation4 =
                CC.unequate relation3 (fromList [3, 4]) (fromList [3, 5])
        it
            "should not, after unequating, consider {3, 4} and {4, 5} as equal anymore" $ do
            equal [3, 4] [3, 5] relation4 `shouldBe` False

equal :: [Int] -> [Int] -> CC.CongruenceClosure -> Bool
equal is1 is2 = CC.equivalent (fromList is1) (fromList is2)

equate :: CC.CongruenceClosure -> [Int] -> [Int] -> CC.CongruenceClosure
equate rel is1 is2 = CC.equate rel (fromList is1) (fromList is2)
