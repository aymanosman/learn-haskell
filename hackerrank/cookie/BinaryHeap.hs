module BinaryHeap where

import qualified Data.List as List
import Test.QuickCheck (quickCheck)
import Control.Monad (replicateM)
import System.Random (randomRIO)

import Heap

-- min heap
data BinaryHeap a =
  Empty
  | Node !a (BinaryHeap a) (BinaryHeap a)
  deriving (Show)

instance Heap BinaryHeap where
  empty = Empty
  isEmpty Empty = True
  isEmpty _ = False

  insert n = merge (singleton n)
  merge Empty h = h
  merge h Empty = h
  merge h@(Node n l r) g@(Node m l' r')
    -- TODO: this a max heap with >
    | n > m     = Node n (merge g l) r
    | otherwise = Node m (merge h l') r'

  findMin (Node n _ _) = Just n
  findMin _ = Nothing
  deleteMin (Node _ l r) = merge l r
  deleteMin _ = empty

singleton :: Ord a => a -> BinaryHeap a
singleton n =
  Node n empty empty
{-# INLINE singleton #-}


-- Conversions
fromList :: Ord a => [a] -> BinaryHeap a
fromList =
  foldr (merge . singleton) Empty

toList :: Ord a => BinaryHeap a -> [a]
toList Empty = []
toList (Node a l r) =
  a : toList (merge l r)

-- Tests
prop_sorted :: [Int] -> Bool
prop_sorted l =
  (toList . fromList) l == List.sort l

main' :: IO ()
main' =
  do putStrLn "==="
     l <- replicateM 10 (randomRIO (1,100)) :: IO [Int]
     print l
     let h = fromList l
     print $ toList h
     print $ findMin h
     print $ toList $ deleteMin h
     putStrLn ""
     putStrLn "=== QuickCheck ==="
     quickCheck prop_sorted
