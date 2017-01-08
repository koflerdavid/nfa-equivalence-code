module Data.Queue where

import qualified Data.List     as List
import Data.Monoid
import qualified Data.Sequence as Seq

class Queue q where
    empty :: q a

    null :: q a -> Bool

    push :: q a -> a -> q a

    pop :: q a -> Maybe (a, q a)

    pushAll :: q a -> [a] -> q a
    pushAll q es = foldr (flip push) q es

newtype FifoQueue a = FifoQueue [a]

instance Queue FifoQueue where
    empty = FifoQueue []
    null (FifoQueue es) = List.null es
    push (FifoQueue es) e = FifoQueue (e : es)
    pop (FifoQueue []) = Nothing
    pop (FifoQueue (e : es)) = Just (e, FifoQueue es)
    pushAll (FifoQueue es) fs = FifoQueue (reverse fs ++ es)

instance Monoid (FifoQueue a) where
    mempty = empty
    mappend (FifoQueue es) (FifoQueue fs) = FifoQueue (fs ++ es)

newtype LifoQueue a = LifoQueue (Seq.Seq a)

instance Queue LifoQueue where
    empty = LifoQueue Seq.empty
    null (LifoQueue es) = Seq.null es
    push (LifoQueue es) e = LifoQueue $ es Seq.|> e
    pop (LifoQueue es) = case Seq.viewl es of
        Seq.EmptyL -> Nothing
        (Seq.:<) e es' -> Just (e, LifoQueue es')
    pushAll (LifoQueue es) fs = LifoQueue (es <> Seq.fromList fs)

instance Monoid (LifoQueue a) where
    mempty = empty
    mappend (LifoQueue es) (LifoQueue fs) = LifoQueue (es <> fs)