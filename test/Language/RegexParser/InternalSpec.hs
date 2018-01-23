module Language.RegexParser.InternalSpec
    ( main
    , spec
    ) where

import Data.Regex
import Data.Regex.Formats            ( MinimallyQuotedRegex(..) )
import Language.RegexParser.Internal

import Control.Monad                 ( forM_ )
import Test.Hspec

main :: IO ()
main = hspec spec

a = Atom 'a'

b = Atom 'b'

spec :: Spec
spec = do
    describe "primitiveRegexParser" $ do
        forM_ primitiveRegexCases $ \(input, expected) -> do
            it ("should parse " ++ show input ++ " to " ++ show (MinimallyQuotedRegex expected)) $ do
                let parsedInput = fmap MinimallyQuotedRegex (tokeniseAndParse primitiveRegex "testcase" input)
                parsedInput `shouldBe` Right (MinimallyQuotedRegex expected)
  where
    primitiveRegexCases = [("a", a), ("'b'", b), ("(a + b)", Alternative a b)]
