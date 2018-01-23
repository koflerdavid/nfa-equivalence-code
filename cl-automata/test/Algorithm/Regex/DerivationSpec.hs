module Algorithm.Regex.DerivationSpec
    ( main
    , spec
    ) where

import Control.Monad              ( forM_ )
import Test.Hspec

import Algorithm.Regex.Derivation
import Data.Regex
import Data.Regex.Formats         ( MinimallyQuotedRegex(..) )


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "derivation of regular expressions" $ do
        let cases =
                [ (Atom 'a', "a", Epsilon)
                , (Atom 'a', "b", Empty)
                , (Sequence (Atom 'a') (Atom 'b'), "a", Atom 'b')
                , (Sequence (Atom 'a') (Atom 'b'), "c", Empty)
                , (Asterisk (Atom 'c'), "c", Asterisk (Atom 'c'))
                ] :: [(Regex Char, String, Regex Char)]
        forM_ cases $ \(from, by, to) -> do
            let shouldConvertFromTo =
                    "(" ++ show (MinimallyQuotedRegex from) ++
                    ")_{" ++ by ++ "} = " ++
                    show (MinimallyQuotedRegex to)
            it shouldConvertFromTo $ do
                MinimallyQuotedRegex (wordDerive by from) `shouldBe` (MinimallyQuotedRegex to)
