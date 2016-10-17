module Language.Automata.HkntParser.Tokeniser where


import Language.Automata.HkntParser.Class

import Control.Monad (forM)
import Data.Char
import Text.Parsec hiding (token)


tokenise :: String -> Either ParseError [(SourcePos, Token)]
tokenise input = do
  let
    -- It is necessary to conserve the line numbers because empty lines will be ignored
    inputLines = zip [1..] (lines input)
    withoutEmptyLines = removeEmptyLines inputLines
  concat <$> forM withoutEmptyLines lineTokeniser


removeEmptyLines :: [(Line, String)] -> [(Line, String)]
removeEmptyLines = filter (not . all isSpace . snd)

lineTokeniser :: (Line, String) -> Either ParseError [(SourcePos, Token)]
lineTokeniser (lineNumber, line) = runParser p () "<input>" line
  where
    p = do
      currentPosition <- getPosition
      setPosition $ currentPosition `setSourceLine` lineNumber
      -- Make sure that all tokens are saved along with their position
      lineTokens <- between spaces spaces $ ((,) <$> getPosition <*> token) `sepEndBy` spaces
      -- Add a newline token at the end of each line.
      newlineToken <- (,) <$> getPosition <*> pure Newline
      return (lineTokens ++ [newlineToken])


token :: Parsec String () Token
token =  try (string "accept" *> notFollowedBy alphaNum *> pure Accept)
     <|> try (string "check" *> notFollowedBy alphaNum *> pure Check)
     <|> Identifier <$> many1 alphaNum
     <|> char ':' *> pure Colon
     <|> try (string "=>" *> pure GreaterEquals)
     <|> char '=' *> pure Equals
     <|> char '-' *> (Arrow <$> (anyChar `sepBy` char '+')) <* string "->"


tokeniseAndParse :: Parsec [(SourcePos, Token)] () a -> SourceName -> String -> Either ParseError a
tokeniseAndParse p source input = tokenise input >>= parse p source
