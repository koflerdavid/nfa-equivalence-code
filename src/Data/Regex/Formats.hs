{-# LANGUAGE FlexibleInstances #-}

module Data.Regex.Formats
    ( FullyQuotedRegex(..)
    , MinimallyQuotedRegex(..)
    ) where

import Data.Regex

import Data.Char   ( isSpace, ord )
import Data.IntSet ( fromList, member )

newtype FullyQuotedRegex c =
    FullyQuotedRegex (Regex c)

instance (Show c) => Show (FullyQuotedRegex c) where
    showsPrec _ (FullyQuotedRegex Empty) = ('∅' :)
    showsPrec _ (FullyQuotedRegex Epsilon) = ('ε' :)
    showsPrec _ (FullyQuotedRegex (Atom c)) = (show c ++)
    showsPrec prec (FullyQuotedRegex (Alternative r s)) =
        let inner =
                showsPrec 6 (FullyQuotedRegex r) .
                (" | " ++) . showsPrec 6 (FullyQuotedRegex s)
        in if prec > 6
               then ('(' :) . inner . (')' :)
               else inner
    showsPrec prec (FullyQuotedRegex (Sequence r s)) =
        let inner =
                showsPrec 7 (FullyQuotedRegex r) .
                (' ' :) . showsPrec 7 (FullyQuotedRegex s)
        in if prec > 7
               then ('(' :) . inner . (')' :)
               else inner
    showsPrec _ (FullyQuotedRegex (Asterisk r)) =
        showsPrec 8 (FullyQuotedRegex r) . ('*' :)

newtype MinimallyQuotedRegex c =
    MinimallyQuotedRegex (Regex c)

instance Show (MinimallyQuotedRegex Char) where
    showsPrec _ (MinimallyQuotedRegex Empty) = ('∅' :)
    showsPrec _ (MinimallyQuotedRegex Epsilon) = ('ε' :)
    showsPrec _ (MinimallyQuotedRegex (Atom c)) =
        if hasToBeEscaped c
            then (show c ++)
            else ([c] ++)
    showsPrec prec (MinimallyQuotedRegex (Alternative r s)) =
        let inner =
                showsPrec 6 (MinimallyQuotedRegex r) .
                (" | " ++) . showsPrec 6 (MinimallyQuotedRegex s)
        in if prec > 6
               then ('(' :) . inner . (')' :)
               else inner
    showsPrec prec (MinimallyQuotedRegex (Sequence r s)) =
        let inner =
                showsPrec 7 (MinimallyQuotedRegex r) .
                (' ' :) . showsPrec 7 (MinimallyQuotedRegex s)
        in if prec > 7
               then ('(' :) . inner . (')' :)
               else inner
    showsPrec _ (MinimallyQuotedRegex (Asterisk r)) =
        showsPrec 8 (MinimallyQuotedRegex r) . ('*' :)

hasToBeEscaped :: Char -> Bool
hasToBeEscaped c = isSpace c || ord c `Data.IntSet.member` charsToBeEscaped
  where
    charsToBeEscaped = fromList . map ord $ "\"'+?*|∅ε()01"
