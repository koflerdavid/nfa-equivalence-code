{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.RegexParserSpec
    ( prop_regexParserRecognizesFullyQuotedRegexes
    , prop_regexParserRecognizesMinimallyQuotedRegexes
    , spec_regexParser
    ) where

import CommonInstances             ()

import Algorithm.Regex.Equivalence ( equivalent )
import Data.Regex
import Data.Regex.Formats          ( toFullyQuotedText, toMinimallyQuotedString,
                                     toMinimallyQuotedText )
import Language.RegexParser

import Control.Monad               ( forM_ )
import Data.Either                 ( fromRight, isRight )
import Test.Hspec
import Test.QuickCheck

a :: Regex Char
a = Atom 'a'

b :: Regex Char
b = Atom 'b'

c :: Regex Char
c = Atom 'c'

ab :: Regex Char
ab = Sequence a b

spec_regexParser :: Spec
spec_regexParser = do
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

prop_regexParserRecognizesMinimallyQuotedRegexes :: Regex Char -> Property
prop_regexParserRecognizesMinimallyQuotedRegexes r =
    let parserInput = toMinimallyQuotedText r
        parseResult = parseRegex "testcase" parserInput
        parsingSuccessful =
            counterexample ("Could not parse " ++ show parserInput) (isRight parseResult)
    in parsingSuccessful .&&. regexEquivalent (fromRight' parseResult) r

prop_regexParserRecognizesFullyQuotedRegexes :: Regex Char -> Property
prop_regexParserRecognizesFullyQuotedRegexes r =
    let parserInput = toFullyQuotedText r
        parseResult = parseRegex "testcase" parserInput
        parsingSuccessful =
            counterexample ("Could not parse " ++ show parserInput) (isRight parseResult)
    in parsingSuccessful .&&. regexEquivalent (fromRight' parseResult) r

regexEquivalent :: (Ord c, Show c) => Regex c -> Regex c -> Property
regexEquivalent r1 r2 = counterexample (show r1 ++ " /== " ++ show r2) $ equivalent r1 r2

fromRight' :: Either a b -> b
fromRight' = fromRight (error "fromRight': Encountered a Left value")
