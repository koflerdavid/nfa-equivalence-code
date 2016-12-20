module Data.Regex
    ( Regex(..)
    , alphabet
    , empty
    , matchesOnlyEmptyString
    , normalised
    ) where

import           Control.Monad.Trans.Writer.Lazy
import qualified Data.Set                       as Set
import Prelude hiding (concat)

data Regex c = Atom c
             | Epsilon
             | Empty
             | Alternative (Regex c) (Regex c)
             | Sequence (Regex c) (Regex c)
             | Asterisk (Regex c)
    deriving (Eq, Ord, Show)

alphabet :: Ord c => Regex c -> Set.Set c
alphabet regex = execWriter (computeAlphabet regex)
  where
    computeAlphabet r = case r of
        Epsilon -> return ()
        Empty -> return ()
        Atom c -> tell (Set.singleton c)

        Asterisk inner -> computeAlphabet inner
        Alternative s t -> computeAlphabet s >> computeAlphabet t
        Sequence s t -> computeAlphabet s >> computeAlphabet t

-- | Determines if the regular expression matches any string.
empty :: Regex c -> Bool
empty Epsilon = False
empty Empty = True
empty (Atom _) = False

empty (Asterisk _) = False -- The empty string is always a match
empty (Alternative r s) =
    empty r && empty s
empty (Sequence r s) = empty r || empty s

matchesOnlyEmptyString :: Regex c -> Bool
matchesOnlyEmptyString Epsilon =
    True
matchesOnlyEmptyString Empty =
    False
matchesOnlyEmptyString (Atom _) =
    False

matchesOnlyEmptyString (Asterisk r) =
    empty r
matchesOnlyEmptyString (Sequence r s) =
    matchesOnlyEmptyString r && matchesOnlyEmptyString s
matchesOnlyEmptyString (Alternative r s) =
    matchesOnlyEmptyString r && matchesOnlyEmptyString s

-- | Taken from "Proof pearl: Regular Expression Equivalence and Relational Algebra"
-- with small modifications from my side
normalised :: (Ord c) => Regex c -> Regex c
normalised Empty = Empty
normalised regex | empty regex = Empty -- Captures Empty -!> Empty, among others

normalised Epsilon = Epsilon
normalised regex | matchesOnlyEmptyString regex =
                   Epsilon -- Captures Epsilon -!> Epsilon, among others
normalised regex@(Atom _) = regex

normalised (Alternative first second) =
    normalised' (normalised first `plus` normalised second)
  where
    Empty `plus` r = r
    r `plus` Empty = r
    Alternative r s `plus` t =
        r `plus` (s `plus` t)
    r `plus` right@(Alternative s t)
        | r == s = right
        | r <= s = Alternative r right
        | otherwise = Alternative s (r `plus` t)
    r `plus` s
        | r == s = r
        | r <= s = Alternative r s
        | otherwise = Alternative s r

normalised (Sequence first second) =
    normalised' (normalised first `concat` normalised second)
  where
    Empty `concat` _ = Empty
    _ `concat` Empty = Empty
    Epsilon `concat` r = r
    r `concat` Epsilon = r
    Sequence r s `concat` t = Sequence r (s `concat` t)
    r `concat` s = Sequence r s

normalised (Asterisk inner@(Asterisk _)) = normalised inner
normalised (Asterisk inner) = Asterisk (normalised inner)

-- | Matches some patterns more. This is most effectively used after another normalisation pass.
normalised' :: (Eq c) => Regex c -> Regex c
normalised' r =
    case r of
        Alternative Epsilon (Sequence a inner@(Asterisk a')) | a == a' -> inner -- 1 + a a* = a*
        Alternative Epsilon inner@(Asterisk _) -> inner
        _ -> r
