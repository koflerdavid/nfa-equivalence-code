module Algorithm.NfaEquivalenceSpec
    ( main
    , spec
    ) where

import           Control.Monad            ( forM_ )
import           Data.IntSet              as IS
import           Test.Hspec

import           Algorithm.NfaEquivalence
import           HkntSamples

main :: IO ()
main = hspec spec

spec :: Spec
spec = forM_ [ ("HK naive", nfaStatesEquivalentHk)
             , ("HK with congruence", nfaStatesEquivalentHkC)
             ] $
    \(name, nfaEquivalent) ->
        describe name $ do
            it "should prove the first example in the HKNT paper as equal" $ do
                let result = nfaEquivalent introductionNfaMerged (IS.singleton 3) (IS.singleton 0)
                result `shouldBe` True

            it "should prove figure 3 in the HKNT paper as equal" $ do
                let result = nfaEquivalent figure3NfaMerged (IS.singleton 0) (IS.singleton 1)
                result `shouldBe` True
