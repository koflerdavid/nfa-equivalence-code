module Language.RegexParser.Class
    ( Token(..)
    , RegexTokenParser
    ) where

import Text.Parsec

data Token = CharToken Char
           | EpsilonToken
           | EmptyToken
           | ZeroOrMoreTimesToken
           | OneOrMoreTimesToken
           | ZeroOrOneTimesToken
           | AlternativeToken
           | LeftParentheseToken
           | RightParentheseToken
    deriving (Eq)

instance Show Token where
    show (CharToken c)        = show c
    show EpsilonToken         = "ε"
    show EmptyToken           = "∅"
    show ZeroOrMoreTimesToken = "*"
    show OneOrMoreTimesToken  = "+"
    show ZeroOrOneTimesToken  = "?"
    show AlternativeToken     = "|"
    show LeftParentheseToken  = "("
    show RightParentheseToken = ")"

type RegexTokenParser u a = Parsec [(SourcePos, Token)] u a
