module Algorithm.Regex.Derivation
    ( derive
    , wordDerive
    ) where

import           Data.Regex

-- This algorithm works as specified in
-- "Partial derivatives of regular expressions and automata constructions",
-- Definition 1.1
derive :: (Ord c) => c -> Regex c -> Regex c
derive _ Epsilon =
    Empty

derive _ r | matchesOnlyEmptyWord r = Empty

derive _ Empty =
    Empty

derive _ r | matchesOnlyEmptyWord r = Empty

derive c (Atom c')
    | c == c' = Epsilon
    | otherwise = Empty

derive c (Alternative r s) =
    normalised $ Alternative (derive c r) (derive c s)

derive c r@(Asterisk inner) =
    normalised $ Sequence (derive c inner) r

derive c (Sequence r s) =
    if (not . matchesEmptyWord) r
    then normalised $ Sequence (derive c r) s
    else normalised $ Alternative (Sequence (derive c r) s) (derive c s)

wordDerive :: (Ord c) => [c] -> Regex c -> Regex c
wordDerive cs r = foldr derive r (reverse cs) -- Use foldr instead of foldl
