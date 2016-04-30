module Heap where

import qualified Data.List as List
import Test.QuickCheck

h =
  insert 9
  $ insert 4
  $ insert 3
  $ merge (singleton 5) (Node 4 (singleton 6) Empty)

main =
  do putStrLn "==="
     print $ toList h
     print $ findMin h
     print $ toList $ deleteMin h
     putStrLn ""
     putStrLn "=== QuickCheck ==="
     quickCheck prop_sorted

-- min heap
data Heap a =
  Empty
  | Node a (Heap a) (Heap a)
  deriving (Show)

singleton n =
  Node n Empty Empty

insert n =
  merge (singleton n)

findMin (Node n _ _) =
  n

deleteMin (Node _ l r) =
  merge l r

merge Empty h = h
merge h Empty = h
merge h@(Node n l r) g@(Node m l' r')
  | n < m     = Node n (merge g l) r
  | otherwise = Node m (merge h l') r'

-- Conversions
fromList :: Ord a => [a] -> Heap a
fromList [] =
  Empty
fromList (x:xs) =
  merge (singleton x) (fromList xs)

toList :: Ord a => Heap a -> [a]
toList Empty = []
toList (Node a l r) =
  a:(toList (merge l r))

-- Tests
prop_sorted :: [Int] -> Bool
prop_sorted l =
  (toList . fromList) l == List.sort l
