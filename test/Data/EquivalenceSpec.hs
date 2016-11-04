module Data.EquivalenceSpec where

import           Control.Monad.Trans.Class
import           Test.Hspec
import           Data.Equivalence.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "union find" $ do
        it "should be possible to merge two equivalence classes" $ do
            runEquivT' $ do
                equate "2" "4"
                equate "6" "8"

                "2" `shouldBeEqualTo` "4"
                "6" `shouldBeEqualTo` "8"
                "2" `shouldNotBeEqualTo` "8"

                equate "4" "6"
                "2" `shouldBeEqualTo` "8"

shouldBeEqualTo :: Ord v => v -> v -> EquivT' s v IO ()
shouldBeEqualTo v1 v2 = do
    result <- equivalent v1 v2
    lift $ result `shouldBe` True

shouldNotBeEqualTo :: Ord v => v -> v -> EquivT' s v IO ()
shouldNotBeEqualTo v1 v2 = do
    result <- equivalent v1 v2
    lift $ result `shouldBe` False
