-- {-# LANGUAGE BangPatterns #-}

module BinaryHeap (
  BinaryHeap(..)
  , fromList, toList
  , size
  , view
  , module Heap
) where

import qualified Data.List as List
import Test.QuickCheck (quickCheck)

import Heap

-- Max Heap
type Size = Int
data BinaryHeap a =
  Empty
  | Node !Size a !(BinaryHeap a) !(BinaryHeap a)
  deriving (Show)

instance Heap BinaryHeap where
  empty = Empty
  isEmpty Empty = True
  isEmpty _ = False

  insert n = merge (singleton n)
  merge Empty h = h
  merge h Empty = h
  merge h@(Node s1 n l r) g@(Node s2 m l' r')
    | n < m     = Node (s1+s2) n (merge g l) r
    | otherwise = Node (s1+s2) m (merge h l') r'

  findMin (Node _ n _ _) = Just n
  findMin _ = Nothing
  deleteMin (Node _ _ l r) = merge l r
  deleteMin _ = empty

size :: BinaryHeap t -> Int
size Empty = 0
size (Node s _ _ _) = s

singleton :: Ord a => a -> BinaryHeap a
singleton n =
  Node 1 n empty empty

view :: Ord a => BinaryHeap a -> Maybe (a, BinaryHeap a)
view Empty = Nothing
view (Node _ a l r) =
  Just (a, merge l r)


-- Conversions
fromList :: Ord a => [a] -> BinaryHeap a
fromList =
  foldr (merge . singleton) Empty

toList :: Ord a => BinaryHeap a -> [a]
toList Empty = []
toList (Node _ a l r) =
  a : toList (merge l r)

-- Tests
prop_sorted :: [Int] -> Bool
prop_sorted l =
  (toList . fromList) l == List.sort l

test :: IO ()
test = quickCheck prop_sorted
