module Data.Regex
    ( Regex(..)
    , alphabet
    , empty
    , matchesEmptyWord
    , matchesOnlyEmptyWord
    , normalised
    ) where

import qualified Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Writer.Lazy
import           Data.Semigroup
import qualified Data.Set                        as Set
import           Prelude                         hiding ( concat )

data Regex c
    = Atom c
    | Epsilon
    | Empty
    | Alternative (Regex c)
                  (Regex c)
    | Sequence (Regex c)
               (Regex c)
    | KleeneStar (Regex c)
    | KleenePlus (Regex c)
    deriving (Eq, Ord, Show)

alphabet :: Ord c => Regex c -> Set.Set c
alphabet regex = execWriter (computeAlphabet regex)
  where
    computeAlphabet r =
        case r of
            Epsilon          -> return ()
            Empty            -> return ()
            Atom c           -> tell (Set.singleton c)
            KleeneStar inner -> computeAlphabet inner
            KleenePlus inner -> computeAlphabet inner
            Alternative s t  -> computeAlphabet s >> computeAlphabet t
            Sequence s t     -> computeAlphabet s >> computeAlphabet t

-- | Determines if the regular expression matches any string.
empty :: Regex c -> Bool
empty Epsilon           = False
empty Empty             = True
empty (Atom _)          = False
empty (KleeneStar _)    = False -- The empty string is always a match
empty (KleenePlus r)    = empty r -- Depends on the inner string.
empty (Alternative r s) = empty r && empty s
empty (Sequence r s)    = empty r || empty s

matchesEmptyWord :: Regex c -> Bool
matchesEmptyWord regex =
    case regex of
        Atom _          -> False
        Epsilon         -> True
        Empty           -> False
        KleeneStar _    -> True
        KleenePlus r    -> matchesEmptyWord r
        Alternative r s -> matchesEmptyWord r || matchesEmptyWord s
        Sequence r s    -> matchesEmptyWord r && matchesEmptyWord s

matchesOnlyEmptyWord :: Regex c -> Bool
matchesOnlyEmptyWord Epsilon = True
matchesOnlyEmptyWord Empty = False
matchesOnlyEmptyWord (Atom _) = False
matchesOnlyEmptyWord (KleeneStar r) = empty r
matchesOnlyEmptyWord (KleenePlus r) = matchesOnlyEmptyWord r
matchesOnlyEmptyWord (Sequence r s) =
    matchesOnlyEmptyWord r && matchesOnlyEmptyWord s
matchesOnlyEmptyWord (Alternative r s) =
    matchesOnlyEmptyWord r && matchesOnlyEmptyWord s

-- | Taken from "Proof pearl: Regular Expression Equivalence and Relational Algebra"
-- with small modifications from my side
normalised :: (Ord c) => Regex c -> Regex c
normalised Empty = Empty
normalised regex
    | empty regex = Empty -- Captures Empty -!> Empty, among others
normalised Epsilon = Epsilon
normalised regex
    | matchesOnlyEmptyWord regex = Epsilon -- Captures Epsilon -!> Epsilon, among others
normalised regex@(Atom _) = regex

normalised (Alternative first second) =
    normalised' (normalised first `plus` normalised second)
  where
    Empty `plus` r = r
    r `plus` Empty = r
    Alternative r s `plus` t = r `plus` (s `plus` t)
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

normalised (KleeneStar inner@(KleeneStar _)) = normalised inner
normalised (KleeneStar (KleenePlus inner)) = KleeneStar (normalised inner)
normalised (KleeneStar inner) = KleeneStar (normalised inner)

normalised (KleenePlus inner@(KleenePlus _)) = normalised inner
normalised (KleenePlus inner)
    | matchesEmptyWord inner = normalised $ KleeneStar (normalised inner)
normalised (KleenePlus inner) = KleenePlus (normalised inner)

-- | Matches some patterns more. This is most effectively used after another normalisation pass.
normalised' :: (Eq c) => Regex c -> Regex c
normalised' r =
    case r of
        Alternative Epsilon (Sequence a inner@(KleeneStar a'))
            | a == a' -> inner -- 1 + a a* = a*
        Alternative Epsilon inner@(KleeneStar _) -> inner -- 1 + a* -> a*
        Alternative Epsilon (KleenePlus inner) -> KleeneStar inner -- 1 + a+ -> a*
        _ -> r

instance Semigroup (Regex c) where
    (<>) = Sequence
    stimes = stimesMonoid

instance Monoid (Regex c) where
    mappend = (<>)
    mempty = Epsilon

instance Functor Regex where
    fmap f r =
        case r of
            Empty             -> Empty
            Epsilon           -> Epsilon
            Atom c            -> Atom (f c)
            KleeneStar inner  -> KleeneStar (fmap f inner)
            KleenePlus inner  -> KleenePlus (fmap f inner)
            Sequence r1 r2    -> Sequence (fmap f r1) (fmap f r2)
            Alternative r1 r2 -> Alternative (fmap f r1) (fmap f r2)
    (<$) c _ = Atom c

instance Applicative Regex where
    pure = Atom
    (<*>) rF a =
        case rF of
            Empty               -> Empty
            Epsilon             -> Epsilon
            Atom f              -> fmap f a
            KleeneStar f        -> KleeneStar (f <*> a)
            KleenePlus f        -> KleenePlus (f <*> a)
            Alternative rF1 rF2 -> Alternative (rF1 <*> a) (rF2 <*> a)
            Sequence rF1 rF2    -> Sequence (rF1 <*> a) (rF2 <*> a)

instance Control.Applicative.Alternative Regex where
    empty = Empty
    (<|>) = Alternative

instance Monad Regex where
    return = pure
    r >>= f =
        case r of
            Empty             -> Empty
            Epsilon           -> Epsilon
            Atom a            -> f a
            KleeneStar inner  -> KleeneStar (inner >>= f)
            KleenePlus inner  -> KleenePlus (inner >>= f)
            Sequence r1 r2    -> Sequence (r1 >>= f) (r2 >>= f)
            Alternative r1 r2 -> Alternative (r1 >>= f) (r2 >>= f)

instance MonadPlus Regex where
    mzero = Empty
    mplus = Alternative
