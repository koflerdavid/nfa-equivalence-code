{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.RegexParserSpec
    ( main
    , spec
    ) where

import Data.Regex
import Data.Regex.Formats   ( toMinimallyQuotedString )
import Language.RegexParser

import Control.Monad        ( forM_ )
import Test.Hspec

main :: IO ()
main = hspec spec

a :: Regex Char
a = Atom 'a'

b :: Regex Char
b = Atom 'b'

c :: Regex Char
c = Atom 'c'

ab :: Regex Char
ab = Sequence a b

spec :: Spec
spec = do
    describe "regexParser" $ do
        forM_ regexCases $ \(input, expected) -> do
            it ("should parse " ++ show input ++ " to " ++ toMinimallyQuotedString expected) $ do
                parseRegex "testcase" input `shouldBe` Right expected
  where
    regexCases =
        [ ("a", a)
        , ("a b ", ab)
        , ("'a' b", ab)
        , ("a", a)
        , ("a + b", Alternative a b)
        , ("a + (b)", Alternative a b)
        , ("a*", KleeneStar a)
        , ("(a)*", KleeneStar a)
        , ("((a)*)*a", KleeneStar (KleeneStar a) `Sequence` a)
        , ("(ab)\x2020", KleenePlus ab)
        , ("((ab)\x2020)\x2020 a", KleenePlus (KleenePlus ab) `Sequence` a)
        , ("ab*", a `Sequence` KleeneStar b)
        , ("(a + b)?", Alternative (Alternative a b) Epsilon)
        , ("ab* +c", Alternative (a `Sequence` KleeneStar b) c)
        , ("1ε0∅", foldr1 Sequence [Epsilon, Epsilon, Empty, Empty])
        ]

instance Show (Regex Char) where
    show = toMinimallyQuotedString
