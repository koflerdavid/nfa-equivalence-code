module Language.Automata.HkntParser.TokeniserSpec (main, spec) where


import HkntSamples
import Language.Automata.HkntParser.Class
import Language.Automata.HkntParser.Tokeniser

import Control.Monad (forM_)
import Test.Hspec
import Test.QuickCheck
import Text.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hkntTokenizer" $ do
    let cases = [ ("a -a-> b c", [Identifier "a", Arrow "a", Identifier "b", Identifier "c", Newline])
                , ("a b -c-> d", [Identifier "a", Identifier "b", Arrow "c", Identifier "d", Newline])
                , ("a -b+c+d-> e f", [Identifier "a", Arrow "bcd", Identifier "e", Identifier "f", Newline])
                , ("accept: a b", [Accept, Colon, Identifier "a", Identifier "b", Newline])
                , ("accept:a b  c ", [Accept, Colon, Identifier "a", Identifier "b", Identifier "c", Newline])
                , ("check: a b => c", [Check, Colon, Identifier "a", Identifier "b", GreaterEquals, Identifier "c", Newline])
                , ("check: a = b c", [Check, Colon, Identifier "a", Equals, Identifier "b", Identifier "c", Newline])
                ]
    forM_ cases $ \ (input, expected) -> do
      it ("should tokenise " ++ show input) $ do
        tokensFromInput input `shouldBe` Right expected

    it "should tokenize the example from the introduction of the HKNT paper" $ do
      -- To make the check readable and the test maintainable, the position information has to be stripped.
      tokensFromInput introductionInHkntFormat `shouldBe` Right [
                                                 Identifier "x", Arrow "a", Identifier "y", Newline,
                                                 Identifier "y", Arrow "a", Identifier "z", Newline,
                                                 Identifier "z", Arrow "a", Identifier "x", Identifier "y", Newline,
                                                 Identifier "u", Arrow "a", Identifier "w", Identifier "v", Newline,
                                                 Identifier "v", Arrow "a", Identifier "w", Newline,
                                                 Identifier "w", Arrow "a", Identifier "u", Newline,
                                                 Accept, Colon, Identifier "y", Identifier "v", Newline,
                                                 Check, Colon, Identifier "x", Equals, Identifier "u", Newline
                                                ]

tokensFromInput :: String -> Either ParseError [Token]
tokensFromInput = fmap (map snd) . tokenise
