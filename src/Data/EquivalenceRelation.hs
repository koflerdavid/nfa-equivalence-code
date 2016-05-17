module Data.EquivalenceRelation
  (
    Eqr
    , empty
    , insert
    , contains
    , equal
    , union
    , fromList
    , partitions
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans.State.Strict

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

data Eqr a = Eqr (Map a Int) Int
                           deriving (Show)

empty :: Eqr a
empty = Eqr M.empty 0

insert :: Ord a => (a, a) -> Eqr a -> Eqr a
insert (x, y) relation@(Eqr classes classCount)
  | x == y = relation
  | otherwise = case (x `M.lookup` classes, y `M.lookup` classes) of
      -- Create a new class.
      (Nothing, Nothing) -> let newClass = M.fromList [(x, classCount), (y, classCount)]
       in Eqr (classes `M.union` newClass) (succ classCount)
      -- Add `x` (`y` respectively) to the other class
      (Just cX,  Nothing) -> Eqr (M.insert y cX classes) classCount
      (Nothing, Just cY) -> Eqr (M.insert x cY classes) classCount

      -- Most complex case: merge two classes. We will implement a bias towards the class of `x`.
      -- `classCount` won't change, but this is fine: it's just a source of fresh equivalence class names.
      (Just cX, Just cY)
        | cX == cY -> relation
        | otherwise -> Eqr classes' classCount
            where classes' = M.map (\c -> if c == cY then cX else c) classes

contains :: Ord a => Eqr a -> (a, a) -> Bool
contains = uncurry . equal

equal :: Ord a => Eqr a -> a -> a -> Bool
equal (Eqr classes _) x y = maybe False id $ do
  classOfA <- x `M.lookup` classes
  classOfB <- y `M.lookup` classes
  return (classOfA == classOfB)

fromList :: Ord a => [(a, a)] -> Eqr a
fromList pairs = undefined

partitions :: Eqr a -> [[a]]
partitions (Eqr classes _) = M.elems $ execState accumulatePartitions M.empty
  -- invert the mapping. The values of it are the partitions.
  where accumulatePartitions = forM_ (M.toList classes) $ \ (x, c) -> do
          modify (\m -> M.insertWith (\[] xs -> (x:xs)) c [] m)

union :: Ord a => Eqr a -> Eqr a -> Eqr a
union (Eqr classes1 counter1) (Eqr classes2 counter2) =
  Eqr mergedClasses (maxCounter + S.size overlap)
    -- Because the names of the classes are only valid within a single relation,
    -- we have to get rid of the overlap before the classes can be safely merged.
    where maxCounter = max counter1 counter2
          overlap = S.intersection (S.fromList $ M.elems classes1) (S.fromList $ M.elems classes2)
          replacementMapping = M.fromList $ S.toList overlap `zip` [maxCounter ..]
          -- Rename overlapping classes. Technically, this could be done even simpler by
          -- adding an offset, but it's better to save equivalence class identifiers.
          classes1' = M.map (\c -> M.findWithDefault c c replacementMapping) classes1
          mergedClasses = classes1' `M.union` classes2
