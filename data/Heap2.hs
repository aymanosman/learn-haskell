module Heap2 where

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

main =
  do
     let h =
           insert 9
           $ insert 4
           $ insert 3
           $ merge (leaf 5) (Node 4 (leaf 6) Empty)
     print h
     print $ findMin h
     print $ deleteMin h
