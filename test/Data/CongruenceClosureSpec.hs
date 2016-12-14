module Data.CongruenceClosureSpec
    ( main
    , spec
    ) where

import           Test.Hspec

import           Data.CongruenceClosure as SIS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "empty family of sets" $ do
        it "should consider equal sets as equal" $ do
            let emptySetOfSets = SIS.empty
            SIS.equal [ 1 ] [ 1 ] emptySetOfSets `shouldBe` True

        it "should not consider any sets as equal" $ do
            let emptySetOfSets = SIS.empty
            SIS.equal [ 2 ] [ 3 ] emptySetOfSets `shouldBe` False

    let relation = SIS.equate SIS.empty [ 1 ] [ 2 ]

    describe "equivalence relation on sets with {1} = {2}" $ do
        it "should consider {1} and {2} as equal" $ do
            SIS.equal [ 1 ] [ 2 ] relation `shouldBe` True

        it "should consider {1} and {1} as equal" $ do
            SIS.equal [ 1 ] [ 1 ] relation `shouldBe` True

        it "should consider {1} and {1, 2} as equal" $ do
            SIS.equal [ 1 ] [ 1, 2 ] relation `shouldBe` True

        it "should not consider {1} and {} as equal" $ do
            SIS.equal [ 1 ] [] relation `shouldBe` False

        it "should not consider {1} and {3} as equal" $ do
            SIS.equal [ 1 ] [ 3 ] relation `shouldBe` False

        it "should not consider {1, 3} and {3} as equal" $ do
            SIS.equal [ 1, 3 ] [ 3 ] relation `shouldBe` False

        it "should not consider {1, 3} and {4} as equal" $ do
            SIS.equal [ 1, 3 ] [ 4 ] relation `shouldBe` False

    describe "equivalence relation on sets with {1} = {2} and {3, 4} = {3, 5}" $ do
        let relation' = SIS.equate relation [ 3, 4 ] [ 3, 5 ]

        it "should consider {3, 4} and {3, 5} as equal" $ do
            SIS.equal [ 3, 4 ] [ 3, 5 ] relation' `shouldBe` True

        it "should consider {1, 3, 4} and {2, 3, 5} as equal" $ do
            SIS.equal [ 1, 3, 4 ] [ 2, 3, 5 ] relation' `shouldBe` True

        it "should not consider {2, 3, 4} and {3, 5} as equal" $ do
            SIS.equal [ 2, 3, 4 ] [ 3, 5 ] relation' `shouldNotBe` True
