module Language.Automata.HkntParser.InternalSpec
    ( main
    , spec
    ) where

import Language.Automata.HkntParser.Class
import Language.Automata.HkntParser.Internal
import Language.Automata.HkntParser.Tokeniser

import Control.Monad                          ( forM_ )
import Test.Hspec
import Text.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "transition parser" $ do
        let cases =
                [ ("a -a-> b", [("a", 'a', "b")])
                , ("a -a-> b\n", [("a", 'a', "b")])
                , ("a -a+b-> b", [("a", 'a', "b"), ("a", 'b', "b")])
                , ("a c -a-> b", [("a", 'a', "b"), ("c", 'a', "b")])
                , ( "a c -a+b-> b"
                  , [ ("a", 'a', "b")
                    , ("a", 'b', "b")
                    , ("c", 'a', "b")
                    , ("c", 'b', "b")
                    ])
                , ( "a -a+b-> b c"
                  , [ ("a", 'a', "b")
                    , ("a", 'a', "c")
                    , ("a", 'b', "b")
                    , ("a", 'b', "c")
                    ])
                , ( "a d -a+b-> b c"
                  , [ ("a", 'a', "b")
                    , ("a", 'a', "c")
                    , ("a", 'b', "b")
                    , ("a", 'b', "c")
                    , ("d", 'a', "b")
                    , ("d", 'a', "c")
                    , ("d", 'b', "b")
                    , ("d", 'b', "c")
                    ])
                ]
        forM_ cases $ \(input, expected) -> do
            it ("should accept " ++ show input) $ do
                transition `shouldAccept` input $ expected
    describe "parser for multiple transitions" $ do
        let cases =
                [ ("a -a-> b\n b -b-> c", [("a", 'a', "b"), ("b", 'b', "c")])
                , ( "a -a-> b\n\n b -b-> c\n\n"
                  , [("a", 'a', "b"), ("b", 'b', "c")])
                , ( "x -a-> y\n\
                    \y -a-> z\n\
                    \z -a-> x y\n\
                    \u -a-> w v\n\
                    \v -a-> w\n\
                    \w -a-> u\n"
                  , expectedTransitions)
                ]
            expectedTransitions =
                [ ("x", 'a', "y")
                , ("y", 'a', "z")
                , ("z", 'a', "x")
                , ("z", 'a', "y")
                , ("u", 'a', "w")
                , ("u", 'a', "v")
                , ("v", 'a', "w")
                , ("w", 'a', "u")
                ]
        forM_ cases $ \(input, expected) -> do
            it ("should accept " ++ show input) $ do
                transitions `shouldAccept` input $ expected
    describe "acceptingStates parser" $ do
        let cases =
                [ ("accept: a   b", ["a", "b"])
                , ("accept: a ", ["a"])
                , ("accept: aa b ", ["aa", "b"])
                , ("accept:  ab", ["ab"])
                ]
        forM_ cases $ \(input, expected) -> do
            it ("should accept " ++ show input) $ do
                acceptingStates `shouldAccept` input $ expected
    describe "checks parser" $ do
        let cases =
                [ ("check: a = b", [(["a"], Equivalence, ["b"])])
                , ("check: a = b\n", [(["a"], Equivalence, ["b"])])
                , ("check: a b = c", [(["a", "b"], Equivalence, ["c"])])
                , ("check: a b => d ef", [(["a", "b"], Inclusion, ["d", "ef"])])
                ]
        forM_ cases $ \(input, expected) -> do
            it ("should accept " ++ show input) $ do
                checks `shouldAccept` input $ expected

shouldAccept ::
       (Eq a, Show a)
    => Parsec [(SourcePos, Token)] () a
    -> String
    -> a
    -> Expectation
shouldAccept parser input expectedResult =
    tokeniseAndParse parser "<input>" input `shouldBe` Right expectedResult
