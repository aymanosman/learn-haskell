module Heap where

import qualified Data.List as List
import Test.QuickCheck (quickCheck)
import Control.Monad (replicateM)
import System.Random (randomRIO)

-- min heap
data Heap a =
  Empty
  | Node !a (Heap a) (Heap a)
  deriving (Show)

empty = Empty

singleton :: a -> Heap a
singleton n =
  Node n empty empty
{-# INLINE singleton #-}

insert :: Ord a => a -> Heap a -> Heap a
insert n =
  merge (singleton n)

findMin :: Heap t -> t
findMin (Node n _ _) =
  n
findMin _ =
  error "empty"

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Node _ l r) =
  merge l r
deleteMin _ =
  error "empty"

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h@(Node n l r) g@(Node m l' r')
  | n < m     = Node n (merge g l) r
  | otherwise = Node m (merge h l') r'

-- Conversions
fromList :: Ord a => [a] -> Heap a
fromList =
  foldr (merge . singleton) Empty

toList :: Ord a => Heap a -> [a]
toList Empty = []
toList (Node a l r) =
  a : toList (merge l r)

-- Tests
prop_sorted :: [Int] -> Bool
prop_sorted l =
  (toList . fromList) l == List.sort l

main :: IO ()
main =
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
