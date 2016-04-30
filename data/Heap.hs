module Heap where

import qualified Data.List as List
import Test.QuickCheck

h =
  insert 9
  $ insert 4
  $ insert 3
  $ merge (leaf 5) (Node 4 (leaf 6) Empty)

main =
  do putStrLn "==="
     print $ toList h
     print $ findMin h
     print $ toList $ deleteMin h
     putStrLn "==="
     quickCheck prop_sorted

-- min heap
data Heap a =
  Empty
  | Node a (Heap a) (Heap a)
  deriving (Show)

leaf n =
  Node n Empty Empty

insert n =
  merge (leaf n)

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
  merge (leaf x) (fromList xs)

toList :: Ord a => Heap a -> [a]
toList Empty = []
toList (Node a l r) =
  a:(toList (merge l r))

-- Tests
prop_sorted :: [Int] -> Bool
prop_sorted l =
  let
    h = fromList l
    l' = toList h
  in
    l' == List.sort l
