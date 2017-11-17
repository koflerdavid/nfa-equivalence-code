module Language.RegexParser.Tokeniser where

import Language.RegexParser.Class

import Data.Char                  ( isSpace )
import Data.Functor               ( ($>) )
import Text.Parsec                hiding ( spaces )

tokenise :: String -> SourceName -> Either ParseError [(SourcePos, Token)]
tokenise = parse regexTokeniser

regexTokeniser :: Parsec String () [(SourcePos, Token)]
regexTokeniser =
    between spaces spaces $
    ((,) <$> getPosition <*> regexToken) `sepEndBy` spaces

regexToken :: Parsec String () Token
regexToken = do
    (oneOf "0∅" $> EmptyToken) <|> (oneOf "1ε" $> EpsilonToken) <|>
        (char '*' $> ZeroOrMoreTimesToken) <|>
        (char '+' $> OneOrMoreTimesToken) <|>
        (char '?' $> ZeroOrOneTimesToken) <|>
        (char '|' $> AlternativeToken) <|>
        (char '(' $> LeftParentheseToken) <|>
        (char ')' $> RightParentheseToken) <|>
        char '\'' *>
        (char '\'' $> EmptyToken <|> CharToken <$> anyChar <* char '\'') <|>
        CharToken <$> anyChar

spaces :: (Monad m) => ParsecT String u m ()
spaces = skipMany $ satisfy isSpace
