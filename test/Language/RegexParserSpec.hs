module Language.RegexParserSpec
    ( main
    , spec
    ) where

import Data.Regex
import Language.RegexParser

import Control.Monad        ( forM_ )
import Test.Hspec

main :: IO ()
main = hspec spec

a = Atom 'a'

b = Atom 'b'

c = Atom 'c'

ab = Sequence a b

spec :: Spec
spec = do
    describe "regexParser" $ do
        forM_ regexCases $ \(input, expected) -> do
            it ("should parse " ++ show input ++ " to " ++ show expected) $ do
                parseRegex "testcase" input `shouldBe` Right expected
  where
    regexCases =
        [ ("a", a)
        , ("a b ", ab)
        , ("'a' b", ab)
        , ("a", a)
        , ("a | b", Alternative a b)
        , ("a | (b)", Alternative a b)
        , ("a*", Asterisk a)
        , ("(a)*", Asterisk a)
        , ("(ab)+", Sequence ab (Asterisk ab))
        , ("ab*", Sequence a (Asterisk b))
        , ("(a | b)?", Alternative (Alternative a b) Epsilon)
        , ("ab* | c", Alternative (Sequence a (Asterisk b)) c)
        , ("1ε0∅", foldr1 Sequence [Epsilon, Epsilon, Empty, Empty])
        ]
