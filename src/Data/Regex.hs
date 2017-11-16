module Data.Regex
    ( Regex(..)
    , alphabet
    , empty
    , matchesEmptyWord
    , matchesOnlyEmptyWord
    , normalised
    ) where

import           Control.Monad.Trans.Writer.Lazy
import qualified Data.Set                        as Set
import           Prelude                         hiding ( concat )

data Regex c = Atom c
             | Epsilon
             | Empty
             | Alternative (Regex c) (Regex c)
             | Sequence (Regex c) (Regex c)
             | Asterisk (Regex c)
    deriving (Eq, Ord)

alphabet :: Ord c => Regex c -> Set.Set c
alphabet regex = execWriter (computeAlphabet regex)
  where
    computeAlphabet r = case r of
        Epsilon         -> return ()
        Empty           -> return ()
        Atom c          -> tell (Set.singleton c)

        Asterisk inner  -> computeAlphabet inner
        Alternative s t -> computeAlphabet s >> computeAlphabet t
        Sequence s t    -> computeAlphabet s >> computeAlphabet t

-- | Determines if the regular expression matches any string.
empty :: Regex c -> Bool
empty Epsilon = False
empty Empty = True
empty (Atom _) = False

empty (Asterisk _) = False -- The empty string is always a match
empty (Alternative r s) =
    empty r && empty s
empty (Sequence r s) = empty r || empty s

matchesEmptyWord :: Regex c -> Bool
matchesEmptyWord regex =
    case regex of
        Atom _          -> False
        Epsilon         -> True
        Empty           -> False
        Asterisk _      -> True
        Alternative r s -> matchesEmptyWord r || matchesEmptyWord s
        Sequence r s    -> matchesEmptyWord r && matchesEmptyWord s

matchesOnlyEmptyWord :: Regex c -> Bool
matchesOnlyEmptyWord Epsilon =
    True
matchesOnlyEmptyWord Empty =
    False
matchesOnlyEmptyWord (Atom _) =
    False

matchesOnlyEmptyWord (Asterisk r) =
    empty r
matchesOnlyEmptyWord (Sequence r s) =
    matchesOnlyEmptyWord r && matchesOnlyEmptyWord s
matchesOnlyEmptyWord (Alternative r s) =
    matchesOnlyEmptyWord r && matchesOnlyEmptyWord s

-- | Taken from "Proof pearl: Regular Expression Equivalence and Relational Algebra"
-- with small modifications from my side
normalised :: (Ord c) => Regex c -> Regex c
normalised Empty = Empty
normalised regex | empty regex = Empty -- Captures Empty -!> Empty, among others

normalised Epsilon = Epsilon
normalised regex | matchesOnlyEmptyWord regex =
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
normalised' r = case r of
    Alternative Epsilon (Sequence a inner@(Asterisk a')) | a == a' -> inner -- 1 + a a* = a*
    Alternative Epsilon inner@(Asterisk _)               -> inner
    _                                                    -> r

instance (Show c) =>
         Show (Regex c) where
    showsPrec _ Empty = ('∅' :)
    showsPrec _ Epsilon = ('ε' :)
    showsPrec _ (Atom c) = (show c ++)
    showsPrec prec (Alternative r s) =
        let inner = showsPrec 6 r . (" | " ++) . showsPrec 6 s
        in
            if prec > 6 then ('(' :) . inner . (')' :) else inner
    showsPrec prec (Sequence r s) =
        let inner = showsPrec 7 r . (' ' :) . showsPrec 7 s
        in
            if prec > 7 then ('(' :) . inner . (')' :) else inner
    showsPrec _ (Asterisk r) =
        showsPrec 8 r . ('*' :)
