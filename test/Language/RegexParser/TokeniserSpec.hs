module Language.RegexParser.TokeniserSpec
    ( main
    , spec
    ) where

import           Language.RegexParser.Class
import           Language.RegexParser.Tokeniser

import           Control.Monad                  ( forM_ )
import           Test.Hspec
import           Text.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    forM_ tokenCases $
        \(input, expected) -> do
            it ("should parse " ++ show input ++ " to " ++ show expected) $ do
                parse regexToken "textcase" input `shouldBe` Right expected
  where
    tokenCases = [ ("a", CharToken 'a') ]
