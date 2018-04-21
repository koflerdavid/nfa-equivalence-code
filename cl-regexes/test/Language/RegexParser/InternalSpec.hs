{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.RegexParser.InternalSpec
    ( spec_regexParserInternal
    ) where

import CommonInstances               ()

import Data.Regex                    ( Regex(..) )
import Data.Regex.Formats            ( toMinimallyQuotedString )
import Language.RegexParser.Internal ( primitiveRegex, tokeniseAndParse )

import Control.Monad                 ( forM_ )
import Test.Hspec

a :: Regex Char
a = Atom 'a'

b :: Regex Char
b = Atom 'b'

spec_regexParserInternal :: Spec
spec_regexParserInternal = do
    describe "primitiveRegexParser" $ do
        forM_ primitiveRegexCases $ \(input, expected) -> do
            it ("should parse " ++ show input ++ " to " ++ toMinimallyQuotedString expected) $ do
                let parsedInput = tokeniseAndParse primitiveRegex "testcase" input
                parsedInput `shouldBe` Right expected
  where
    primitiveRegexCases = [("a", a), ("'b'", b), ("(a + b)", Alternative a b)]
