module IntHeap where

import qualified Data.List as List
import Test.QuickCheck (quickCheck)
import Control.Monad (replicateM)
import System.Random (randomRIO)

data Heap =
  Empty
  | Node
    { _size :: {-# UNPACK #-} !Int
    , _val :: {-# UNPACK #-} !Int
    , _left :: Heap
    , _right :: Heap
    }

instance Show Heap where
  show h =
    "fromList " ++ show (toList h)

empty :: Heap
empty = Empty

singleton :: Int -> Heap
singleton n =
  Node 1 n empty empty
{-# INLINE singleton #-}

size :: Heap -> Int
size Empty = 0
size heap = _size heap

insert :: Int -> Heap -> Heap
insert n =
  merge (singleton n)

view :: Heap -> Maybe (Int, Heap)
view (Node _size n l r) = Just (n, merge l r)
view _ = Nothing
{-# INLINE view #-}

viewHead :: Heap -> Maybe Int
viewHead Empty = Nothing
viewHead heap = Just $ _val heap

deleteMin :: Heap -> Heap
deleteMin Empty = error "empty"
deleteMin h =
  merge (_left h) (_right h)

merge :: Heap -> Heap -> Heap
merge Empty h = h
merge h Empty = h
merge h g
 | _val h < _val g = merge' h g
 | otherwise = merge' g h

merge' :: Heap -> Heap -> Heap
merge' h g =
  Node
    (_size h + _size g)
    (_val h)
    (merge g (_left h))
    (_right h)
{-# INLINE merge' #-}

-- Conversions
fromList :: [Int] -> Heap
fromList =
  foldr (merge . singleton) Empty

toList :: Heap -> [Int]
toList Empty = []
toList (Node _ a l r) =
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
     print $ view h
     print $ toList $ deleteMin h
     putStrLn ""
     putStrLn "=== QuickCheck ==="
     quickCheck prop_sorted
