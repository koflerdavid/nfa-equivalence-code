module Data.Queue where

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import qualified Data.Sequence      as Seq

class Queue q where
    empty :: q a
    null :: q a -> Bool
    push :: q a -> a -> q a
    pop :: q a -> Maybe (a, q a)
    pushAll :: q a -> [a] -> q a
    pushAll = foldr (flip push)
    singleton :: a -> q a

newtype FifoQueue a =
    FifoQueue [a]
    deriving (Eq, Show)

instance Queue FifoQueue where
    empty = FifoQueue []
    null (FifoQueue es) = List.null es
    push (FifoQueue es) e = FifoQueue (e : es)
    pop (FifoQueue [])     = Nothing
    pop (FifoQueue (e:es)) = Just (e, FifoQueue es)
    pushAll (FifoQueue es) fs = FifoQueue (reverse fs ++ es)
    singleton = FifoQueue . (: [])

instance Semigroup (FifoQueue a) where
    FifoQueue es <> FifoQueue fs = FifoQueue (fs <> es)
    sconcat queues = FifoQueue $ sconcat (NonEmpty.map (\(FifoQueue es) -> es) queues)

instance Monoid (FifoQueue a) where
    mempty = empty
    mappend = (<>)

newtype LifoQueue a =
    LifoQueue (Seq.Seq a)
    deriving (Eq, Show)

instance Queue LifoQueue where
    empty = LifoQueue Seq.empty
    null (LifoQueue es) = Seq.null es
    push (LifoQueue es) e = LifoQueue $ es Seq.|> e
    pop (LifoQueue es) =
        case Seq.viewl es of
            Seq.EmptyL   -> Nothing
            e Seq.:< es' -> Just (e, LifoQueue es')
    pushAll (LifoQueue es) fs = LifoQueue (es <> Seq.fromList fs)
    singleton = LifoQueue . Seq.singleton

instance Semigroup (LifoQueue a) where
    LifoQueue es <> LifoQueue fs = LifoQueue (es <> fs)
    sconcat queues = LifoQueue $ sconcat (NonEmpty.map (\(LifoQueue es) -> es) queues)

instance Monoid (LifoQueue a) where
    mempty = empty
    mappend = (<>)
