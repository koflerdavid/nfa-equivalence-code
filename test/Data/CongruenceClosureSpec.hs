module Data.CongruenceClosureSpec
    ( main
    , spec
    ) where

import qualified Data.CongruenceClosure as CC

import qualified Data.IntSet            as IS
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "empty family of sets" $ do
        it "should consider equal sets as equal" $ do
            let emptySetOfSets = CC.empty
            equal [ 1 ] [ 1 ] emptySetOfSets `shouldBe` True

        it "should not consider any sets as equal" $ do
            let emptySetOfSets = CC.empty
            equal [ 2 ] [ 3 ] emptySetOfSets `shouldBe` False

    let relation = equate CC.empty [ 1 ] [ 2 ]

    describe "equivalence relation on sets with {1} = {2}" $ do
        it "should consider {1} and {2} as equal" $ do
            equal [ 1 ] [ 2 ] relation `shouldBe` True

        it "should consider {1} and {1} as equal" $ do
            equal [ 1 ] [ 1 ] relation `shouldBe` True

        it "should consider {1} and {1, 2} as equal" $ do
            equal [ 1 ] [ 1, 2 ] relation `shouldBe` True

        it "should not consider {1} and {} as equal" $ do
            equal [ 1 ] [] relation `shouldBe` False

        it "should not consider {1} and {3} as equal" $ do
            equal [ 1 ] [ 3 ] relation `shouldBe` False

        it "should not consider {1, 3} and {3} as equal" $ do
            equal [ 1, 3 ] [ 3 ] relation `shouldBe` False

        it "should not consider {1, 3} and {4} as equal" $ do
            equal [ 1, 3 ] [ 4 ] relation `shouldBe` False

    describe "equivalence relation on sets with {1} = {2} and {3, 4} = {3, 5}" $ do
        let relation' = equate relation [ 3, 4 ] [ 3, 5 ]

        it "should consider {3, 4} and {3, 5} as equal" $ do
            equal [ 3, 4 ] [ 3, 5 ] relation' `shouldBe` True

        it "should consider {1, 3, 4} and {2, 3, 5} as equal" $ do
            equal [ 1, 3, 4 ] [ 2, 3, 5 ] relation' `shouldBe` True

        it "should not consider {2, 3, 4} and {3, 5} as equal" $ do
            equal [ 2, 3, 4 ] [ 3, 5 ] relation' `shouldNotBe` True

    describe "deleteting a rule from a congruence closure" $ do
        let relation' = equate relation [ 3, 4 ] [ 3, 5 ]

        it "should consider {3, 4} and {3, 5} as equal" $ do
            equal [ 3, 4 ] [ 3, 5 ] relation' `shouldBe` True

        let relation'' = CC.unequate relation' (IS.fromList [3, 4]) (IS.fromList [3, 5])

        it "should not, after unequating, consider {3, 4} and {4, 5} as equal anymore" $ do
            equal [ 3, 4 ] [ 3, 5 ] relation'' `shouldBe` False

equal :: [Int] -> [Int] -> CC.CongruenceClosure -> Bool
equal is1 is2 rel = CC.equivalent (IS.fromList is1) (IS.fromList is2) rel

equate :: CC.CongruenceClosure -> [Int] -> [Int] -> CC.CongruenceClosure
equate rel is1 is2 = CC.equate rel (IS.fromList is1) (IS.fromList is2)
