module Language.RegexParser.Tokeniser where

import Language.RegexParser.Class

import           Data.Char        ( isSpace )
import           Data.Functor     ( ($>) )
import qualified Data.Text        as T
import           Text.Parsec      hiding ( spaces )

tokenise :: SourceName -> T.Text -> Either ParseError [(SourcePos, Token)]
tokenise = parse regexTokeniser

regexTokeniser :: Parsec T.Text () [(SourcePos, Token)]
regexTokeniser =
    between spaces spaces $
    ((,) <$> getPosition <*> regexToken) `sepEndBy` spaces

regexToken :: Parsec T.Text () Token
regexToken = do
    (oneOf "0∅" $> EmptyToken) <|> (oneOf "1ε" $> EpsilonToken) <|>
        (char '*' $> ZeroOrMoreTimesToken) <|>
        (char '\x2020' $> OneOrMoreTimesToken) <|>
        (char '?' $> ZeroOrOneTimesToken) <|>
        (char '+' $> AlternativeToken) <|>
        (char '(' $> LeftParentheseToken) <|>
        (char ')' $> RightParentheseToken) <|>
        char '\'' *>
        (char '\'' $> EmptyToken <|> CharToken <$> anyChar <* char '\'') <|>
        CharToken <$> anyChar

spaces :: (Monad m) => ParsecT T.Text u m ()
spaces = skipMany $ satisfy isSpace
