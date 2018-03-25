{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.RegexParser.InternalSpec
    ( main
    , spec
    ) where

import Data.Regex
import Data.Regex.Formats            ( toMinimallyQuotedString )
import Language.RegexParser.Internal

import Control.Monad                 ( forM_ )
import Test.Hspec

main :: IO ()
main = hspec spec

a :: Regex Char
a = Atom 'a'

b :: Regex Char
b = Atom 'b'

spec :: Spec
spec = do
    describe "primitiveRegexParser" $ do
        forM_ primitiveRegexCases $ \(input, expected) -> do
            it ("should parse " ++ show input ++ " to " ++ toMinimallyQuotedString expected) $ do
                let parsedInput = tokeniseAndParse primitiveRegex "testcase" input
                parsedInput `shouldBe` Right expected
  where
    primitiveRegexCases = [("a", a), ("'b'", b), ("(a + b)", Alternative a b)]

instance Show (Regex Char) where
    show = toMinimallyQuotedString
