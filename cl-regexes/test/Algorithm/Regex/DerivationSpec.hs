{-# LANGUAGE FlexibleInstances #-}

module Algorithm.Regex.DerivationSpec
    ( spec_regexDerivation
    ) where

import CommonInstances            ()

import Algorithm.Regex.Derivation ( wordDerive )
import Data.Regex                 ( Regex(..) )
import Data.Regex.Formats         ( toMinimallyQuotedString )

import Control.Monad              ( forM_ )
import Data.Monoid                ( mconcat )
import Test.Hspec

spec_regexDerivation :: Spec
spec_regexDerivation = do
    describe "derivation of regular expressions" $ do
        let cases =
                [ (Atom 'a', "a", Epsilon)
                , (Atom 'a', "b", Empty)
                , (Sequence (Atom 'a') (Atom 'b'), "a", Atom 'b')
                , (Sequence (Atom 'a') (Atom 'b'), "c", Empty)
                , (KleeneStar (Atom 'c'), "c", KleeneStar (Atom 'c'))
                ]
        forM_ cases $ \(from, by, to) -> do
            let shouldConvertFromTo =
                    mconcat
                        [ "("
                        , toMinimallyQuotedString from
                        , ")_{"
                        , by
                        , "} = "
                        , toMinimallyQuotedString to
                        ]
            it shouldConvertFromTo $ do wordDerive by from `shouldBe` to
