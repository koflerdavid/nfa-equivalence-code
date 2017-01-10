module Language.RegexParser.InternalSpec
    ( main
    , spec
    ) where

import           Data.Regex
import           Language.RegexParser.Internal

import           Control.Monad                  ( forM_ )
import           Test.Hspec

main :: IO ()
main = hspec spec

a = Atom 'a'

b = Atom 'b'

spec :: Spec
spec = do
    describe "primitiveRegexParser" $ do
        forM_ primitiveRegexCases $
            \(input, expected) -> do
                it ("should parse " ++ show input ++ " to " ++ show expected) $ do
                    tokeniseAndParse primitiveRegex "testcase" input `shouldBe` Right expected
  where
    primitiveRegexCases = [ ("a", a), ("'b'", b), ("(a | b)", Alternative a b) ]
