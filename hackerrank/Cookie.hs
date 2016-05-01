module Main where

import qualified Data.List as List
import Debug.Trace (trace)
-- import qualified Data.Heap as H
import Test.QuickCheck  -- (quickCheck)

data Ans =
  Done Heap
  | More Heap
  | Fail

main, mai' :: IO ()
mai' =
  do let readInts = fmap read `fmap` words `fmap` getLine :: IO [Int]
     [_, sweetness] <- readInts
     cs <- readInts
     -- assert length cs == _ above
     print $ ans sweetness cs

main =
  do putStrLn "==="
     print $ ans (10^7) [1,2,3,9,10,12]
     print $ ans 0 [1,2]
     putStrLn "=== QuickCheck ==="
     quickCheck prop_same_as_naive

ans :: Int -> [Int] -> Int
ans m l =
  case ans' 0 m (fromList l) of
    Nothing -> -1
    Just (n, _) -> n

ans' :: Int -> Int -> Heap -> Maybe (Int, Heap)
ans' n sweetness h =
  case step sweetness h of
    Fail -> Nothing

    Done heap ->
      Just (n, heap)

    More heap ->
      ans' (n+1) sweetness heap

step :: Int -> Heap -> Ans
step m heap
  | size heap < 2 =
    case viewHead heap of
      Nothing -> Fail
      Just n ->
        if n >= m
        then Done heap
        else Fail
  | otherwise =
    let Just (a, h) = view heap
        Just (b, h') = view h
    in
      if a >= m && b >= m
      then Done heap
      else More $ insert (a+2*b) h'

-- tests

naive :: Int -> Int -> [Int] -> Int
naive n m l
  | not (null l) && all (>=m) l = -- expect sorted list
    n
  | otherwise =
    case l of
      a:b:xs ->
        let l' = List.sort ((a+2*b):xs)
        in
        naive (n+1) m
        -- $ trace ("l: " ++ show l')
        l'

      _ -> -1

prop_same_as_naive :: NonNegative Int -> NonEmptyList (Positive Int) -> Bool
prop_same_as_naive m' l' =
  let
    m = getNonNegative m'
    l = map getPositive (getNonEmpty l')
  in
  ans m l == naive 0 m (List.sort l)

-- min heap implementation

data Heap =
  Empty
  | Node
    { _size :: !Int
    , _val :: !Int
    , _left :: Heap
    , _right :: Heap
    }
  deriving (Show)

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
