module Language.RegexParser.Tokeniser where

import           Language.RegexParser.Class

import           Data.Char                  ( isSpace )
import           Text.Parsec                hiding ( spaces )

tokenise :: String -> SourceName -> Either ParseError [(SourcePos, Token)]
tokenise source input = parse regexTokeniser source input

regexTokeniser :: Parsec String () [(SourcePos, Token)]
regexTokeniser = between spaces spaces $ ((,) <$> getPosition <*> regexToken) `sepBy` spaces

regexToken :: Parsec String () Token
regexToken = do
    char '0' *> pure EmptyToken
        <|> char '1' *> pure EpsilonToken
        <|> char '*' *> pure ZeroOrMoreTimesToken
        <|> char '+' *> pure OneOrMoreTimesToken
        <|> char '?' *> pure ZeroOrOneTimesToken
        <|> char '|' *> pure AlternativeToken
        <|> char '(' *> pure LeftParentheseToken
        <|> char ')' *> pure RightParentheseToken
        <|> char '\'' *>
            (char '\'' *> pure EmptyToken <|> CharToken <$> anyChar <* char '\'')
        <|> CharToken <$> anyChar

spaces :: (Monad m) => ParsecT String u m ()
spaces = skipMany $ satisfy isSpace
