{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algorithm.Regex.DerivationSpec
    ( spec_regexDerivation
    ) where

import Control.Monad              ( forM_ )
import Data.Monoid                ( mconcat )
import Test.Hspec

import Algorithm.Regex.Derivation
import Data.Regex
import Data.Regex.Formats         ( toMinimallyQuotedString )

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

instance Show (Regex Char) where
    show = toMinimallyQuotedString
