{-# LANGUAGE LambdaCase #-}

module Language.RegexParser.Internal where

import           Data.Regex
import           Language.RegexParser.Class
import           Language.RegexParser.Tokeniser

import           Data.Bifunctor                 ( first )
import           Data.Functor                   ( ($>) )
import           Data.Functor.Identity
import qualified Data.Text                      as T
import           Text.Parsec                    hiding ( Empty )
import           Text.Parsec.Expr

tokeniseAndParse ::
       RegexTokenParser () a -> SourceName -> T.Text -> Either String a
tokeniseAndParse parser name input =
    first show $ tokenise name input >>= parse parser name

regexOperatorTable :: OperatorTable [(SourcePos, Token)] u Identity (Regex Char)
regexOperatorTable =
    [ [ Postfix $ justToken ZeroOrMoreTimesToken $> Asterisk -- ZeroOrMore
      , Postfix $ justToken OneOrMoreTimesToken $> oneOrMore -- OneOrMore
      , Postfix $ justToken ZeroOrOneTimesToken $> zeroOrMore -- ZeroOrOne
      ]
    , [Infix (pure Sequence) AssocRight] -- Sequence
    , [Infix (justToken AlternativeToken $> Alternative) AssocRight] -- Alternative
    ]

zeroOrMore :: Regex c -> Regex c
zeroOrMore r = Alternative r Epsilon

oneOrMore :: Regex c -> Regex c
oneOrMore r = Sequence r (Asterisk r)

regex :: RegexTokenParser () (Regex Char)
regex = buildExpressionParser regexOperatorTable primitiveRegex

primitiveRegex :: RegexTokenParser () (Regex Char)
primitiveRegex =
    atom <|> (justToken EpsilonToken $> Epsilon) <|>
    (justToken EmptyToken $> Empty) <|>
    between
        (justToken LeftParentheseToken)
        (justToken RightParentheseToken)
        regex

atom :: RegexTokenParser () (Regex Char)
atom =
    tokenFromPair
        (\case
             (CharToken c) -> Just (Atom c)
             _ -> Nothing)

justToken :: Token -> RegexTokenParser u ()
justToken t =
    tokenFromPair
        (\t' ->
             if t' == t
                 then Just ()
                 else Nothing)

tokenFromPair :: (Token -> Maybe a) -> RegexTokenParser u a
tokenFromPair testTok = token (show . snd) fst (testTok . snd)
