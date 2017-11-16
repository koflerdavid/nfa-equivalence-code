{-# LANGUAGE LambdaCase #-}

module Language.Automata.HkntParser.Internal where

import Language.Automata.HkntParser.Class

import Text.Parsec                        hiding ( label, labels )

transitions :: Parsec [(SourcePos, Token)] () [Transition]
transitions = concat <$> transition `sepEndBy` newlineToken

transition :: Parsec [(SourcePos, Token)] () [Transition]
transition = do
    origins <- many1 identifier
    labels <- transitionArrow
    destinations <- many1 identifier
    return [ (origin, label, destination)
           | origin <- origins
           , label <- labels
           , destination <- destinations ]

acceptingStates :: Parsec [(SourcePos, Token)] () [String]
acceptingStates = option [] $ acceptKeyword *> colonToken *> many1 identifier <* newlineToken

checks :: Parsec [(SourcePos, Token)] () [Check String]
checks = check `sepEndBy` newlineToken

check :: Parsec [(SourcePos, Token)] () (Check String)
check = checkKeyword *> colonToken *> ((,,) <$> many1 identifier <*> operation <*> many1 identifier)
  where
    operation = equalsOperator *> pure Equivalence <|> greaterEqualsOperator *> pure Inclusion

-- Parser combinators for single tokens
identifier :: Parsec [(SourcePos, Token)] s String
identifier = token (show . snd)
                   fst
                   (\case
                        (_, Identifier name) -> Just name
                        _ -> Nothing)

acceptKeyword :: Parsec [(SourcePos, Token)] s ()
acceptKeyword = token (show . snd)
                      fst
                      (\case
                           (_, Accept) -> Just ()
                           _ -> Nothing)

checkKeyword :: Parsec [(SourcePos, Token)] s ()
checkKeyword = token (show . snd)
                     fst
                     (\case
                          (_, Check) -> Just ()
                          _ -> Nothing)

colonToken :: Parsec [(SourcePos, Token)] s ()
colonToken = token (show . snd)
                   fst
                   (\case
                        (_, Colon) -> Just ()
                        _ -> Nothing)

equalsOperator :: Parsec [(SourcePos, Token)] s ()
equalsOperator = token (show . snd)
                       fst
                       (\case
                            (_, Equals) -> Just ()
                            _ -> Nothing)

greaterEqualsOperator :: Parsec [(SourcePos, Token)] s ()
greaterEqualsOperator = token (show . snd)
                              fst
                              (\case
                                   (_, GreaterEquals) -> Just ()
                                   _ -> Nothing)

transitionArrow :: Parsec [(SourcePos, Token)] s [Char]
transitionArrow = token (show . snd)
                        fst
                        (\case
                             (_, Arrow labels) -> Just labels
                             _ -> Nothing)

newlineToken :: Parsec [(SourcePos, Token)] s ()
newlineToken = token (show . snd)
                     fst
                     (\case
                          (_, Newline) -> Just ()
                          _ -> Nothing)
