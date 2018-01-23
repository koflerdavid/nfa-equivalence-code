module Algorithm.NfaEquivalenceSpec
    ( main
    , spec
    ) where

import Algorithm.NfaEquivalence
import HkntSamples

import Control.Monad            ( forM_ )
import Data.IntSet              ( singleton )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    forM_
        [ ("HK naive", nfaStatesEquivalentHk)
        , ("HK with congruence", nfaStatesEquivalentHkC)
        ] $ \(name, nfaEquivalent) ->
        describe name $ do
            it "should prove the first example in the HKNT paper as equal" $ do
                let result =
                        nfaEquivalent
                            introductionNfaMerged
                            (singleton 3)
                            (singleton 0)
                result `shouldBe` True
            it "should prove figure 3 in the HKNT paper as equal" $ do
                let result =
                        nfaEquivalent
                            figure3NfaMerged
                            (singleton 0)
                            (singleton 1)
                result `shouldBe` True
