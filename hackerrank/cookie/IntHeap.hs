module IntHeap where

import qualified Data.List as List
import Test.QuickCheck (quickCheck)
import Control.Monad (replicateM)
import System.Random (randomRIO)

data IntHeap =
  Empty
  | Node
    { _size :: {-# UNPACK #-} !Int
    , _val :: {-# UNPACK #-} !Int
    , _left :: IntHeap
    , _right :: IntHeap
    }

instance Show IntHeap where
  show h =
    "fromList " ++ show (toList h)

empty :: IntHeap
empty = Empty

singleton :: Int -> IntHeap
singleton n =
  Node 1 n empty empty
{-# INLINE singleton #-}

size :: IntHeap -> Int
size Empty = 0
size heap = _size heap

insert :: Int -> IntHeap -> IntHeap
insert n =
  merge (singleton n)

view :: IntHeap -> Maybe (Int, IntHeap)
view (Node _size n l r) = Just (n, merge l r)
view _ = Nothing
{-# INLINE view #-}

viewHead :: IntHeap -> Maybe Int
viewHead Empty = Nothing
viewHead heap = Just $ _val heap

deleteMin :: IntHeap -> IntHeap
deleteMin Empty = error "empty"
deleteMin h =
  merge (_left h) (_right h)

merge :: IntHeap -> IntHeap -> IntHeap
merge Empty h = h
merge h Empty = h
merge h g
 | _val h < _val g = merge' h g
 | otherwise = merge' g h

merge' :: IntHeap -> IntHeap -> IntHeap
merge' h g =
  Node
    (_size h + _size g)
    (_val h)
    (merge g (_left h))
    (_right h)
{-# INLINE merge' #-}

-- Conversions
fromList :: [Int] -> IntHeap
fromList =
  foldr (merge . singleton) Empty

toList :: IntHeap -> [Int]
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
