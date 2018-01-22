module Data.Regex.Formats
    ( FullyQuotedRegex(..)
    ) where

import Data.Regex

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
