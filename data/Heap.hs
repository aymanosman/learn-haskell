module Heap where

import qualified Data.List as List
import Test.QuickCheck

h1 :: Heap Integer
h1 =
  insert 9
  $ insert 4
  $ insert 3
  $ merge (singleton 5) (Node 4 (singleton 6) Empty)

main :: IO ()
main =
  do putStrLn "==="
     print $ toList h1
     print $ findMin h1
     print $ toList $ deleteMin h1
     putStrLn ""
     putStrLn "=== QuickCheck ==="
     quickCheck prop_sorted

-- min heap
data Heap a =
  Empty
  | Node a (Heap a) (Heap a)
  deriving (Show)

singleton :: a -> Heap a
singleton n =
  Node n Empty Empty

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
  a:toList (merge l r)

-- Tests
prop_sorted :: [Int] -> Bool
prop_sorted l =
  (toList . fromList) l == List.sort l
