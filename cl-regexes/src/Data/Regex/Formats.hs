{-# LANGUAGE FlexibleInstances #-}

module Data.Regex.Formats
    ( toFullyQuotedString
    , toFullyQuotedText
    , toMinimallyQuotedString
    , toMinimallyQuotedText
    ) where

import           Data.Regex

import           Data.Char   ( isSpace, ord )
import           Data.IntSet ( fromList, member )
import qualified Data.Text   as T

toFullyQuotedString :: Regex Char -> String
toFullyQuotedString = show . FullyQuotedRegex

toFullyQuotedText :: Regex Char -> T.Text
toFullyQuotedText = T.pack . toFullyQuotedString

toMinimallyQuotedString :: Regex Char -> String
toMinimallyQuotedString = show . MinimallyQuotedRegex

toMinimallyQuotedText :: Regex Char -> T.Text
toMinimallyQuotedText = T.pack . toMinimallyQuotedString

newtype FullyQuotedRegex c =
    FullyQuotedRegex (Regex c)
    deriving (Eq, Ord)

instance (Show c) => Show (FullyQuotedRegex c) where
    showsPrec _ (FullyQuotedRegex Empty) = showChar '∅'
    showsPrec _ (FullyQuotedRegex Epsilon) = showChar 'ε'
    showsPrec _ (FullyQuotedRegex (Atom c)) = shows c
    showsPrec prec (FullyQuotedRegex (Alternative r s)) =
        showParen (prec > 6) $
            showsPrec 6 (FullyQuotedRegex r) .
            showString " + " .
            showsPrec 6 (FullyQuotedRegex s)
    showsPrec prec (FullyQuotedRegex (Sequence r s)) =
        showParen (prec > 7) $
            showsPrec 7 (FullyQuotedRegex r) .
            showChar ' ' .
            showsPrec 7 (FullyQuotedRegex s)
    showsPrec prec (FullyQuotedRegex (KleeneStar r)) =
        showParen (prec >= 8) $
            showsPrec 8 (FullyQuotedRegex r) . showChar '*'
    showsPrec prec (FullyQuotedRegex (KleenePlus r)) =
        showParen (prec >= 8) $
            showsPrec 8 (FullyQuotedRegex r) . showChar '†'

newtype MinimallyQuotedRegex c =
    MinimallyQuotedRegex (Regex c)
    deriving (Eq, Ord)

instance Show (MinimallyQuotedRegex Char) where
    showsPrec _ (MinimallyQuotedRegex Empty) = showChar '∅'
    showsPrec _ (MinimallyQuotedRegex Epsilon) = showChar 'ε'
    showsPrec _ (MinimallyQuotedRegex (Atom c)) =
        if hasToBeEscaped c
            then shows c
            else showChar c
    showsPrec prec (MinimallyQuotedRegex (Alternative r s)) =
        showParen (prec > 6) $
            showsPrec 6 (MinimallyQuotedRegex r) .
            showString " + " .
            showsPrec 6 (MinimallyQuotedRegex s)
    showsPrec prec (MinimallyQuotedRegex (Sequence r s)) =
        showParen (prec > 7) $
            showsPrec 7 (MinimallyQuotedRegex r) .
            showChar ' ' .
            showsPrec 7 (MinimallyQuotedRegex s)
    showsPrec prec (MinimallyQuotedRegex (KleeneStar r)) =
        showParen (prec >= 8) $
            showsPrec 8 (MinimallyQuotedRegex r) . showChar '*'
    showsPrec prec (MinimallyQuotedRegex (KleenePlus r)) =
        showParen (prec >= 8) $
            showsPrec 8 (MinimallyQuotedRegex r) . showChar '†'

hasToBeEscaped :: Char -> Bool
hasToBeEscaped c = isSpace c || ord c `Data.IntSet.member` charsToBeEscaped
  where
    charsToBeEscaped = fromList . map ord $ "\"'+?*∅ε()01\x2020"
