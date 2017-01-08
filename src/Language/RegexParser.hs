{-# LANGUAGE LambdaCase #-}

module Language.RegexParser where

import           Data.Regex
import           Language.RegexParser.Class
import           Language.RegexParser.Tokeniser

import           Data.Either.Combinators        ( mapLeft )
import           Data.Functor.Identity
import           Text.Parsec                    hiding ( Empty )
import           Text.Parsec.Expr

parseRegex :: SourceName -> String -> Either String (Regex Char)
parseRegex = tokeniseAndParse regex

tokeniseAndParse :: RegexTokenParser () a -> SourceName -> String -> Either String a
tokeniseAndParse parser name input =
    mapLeft show $ tokenise name input >>= parse parser name

regexOperatorTable :: OperatorTable [(SourcePos, Token)] u Identity (Regex Char)
regexOperatorTable = [ [ Postfix $ justToken ZeroOrMoreTimesToken *> pure Asterisk -- ZeroOrMore
                       , Postfix $ justToken OneOrMoreTimesToken *> pure oneOrMore -- OneOrMore
                       , Postfix $ justToken ZeroOrOneTimesToken *> pure zeroOrMore -- ZeroOrOne
                       ]
                     , [ Infix (pure Sequence) AssocRight ] -- Sequence
                     , [ Infix (justToken AlternativeToken *> pure Alternative) AssocRight ] -- Alternative
                     ]

zeroOrMore :: Regex c -> Regex c
zeroOrMore r = Alternative r Epsilon

oneOrMore :: Regex c -> Regex c
oneOrMore r = Sequence r (Asterisk r)

regex :: RegexTokenParser () (Regex Char)
regex = buildExpressionParser regexOperatorTable primitiveRegex

primitiveRegex :: RegexTokenParser () (Regex Char)
primitiveRegex = atom
    <|> justToken EpsilonToken *> pure Epsilon
    <|> justToken EmptyToken *> pure Empty
    <|> between (justToken LeftParentheseToken) (justToken RightParentheseToken) regex

atom :: RegexTokenParser () (Regex Char)
atom = tokenFromPair (\case
                          (CharToken c) -> Just (Atom c)
                          _ -> Nothing)

justToken :: Token -> RegexTokenParser u ()
justToken t = tokenFromPair (\t' -> if t' == t then Just () else Nothing)

tokenFromPair :: (Token -> Maybe a) -> RegexTokenParser u a
tokenFromPair testTok = token (show . snd) fst (testTok . snd)