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
  | Node !Int !Int Heap Heap -- size then val, change to record for clarity
  deriving (Show)

empty :: Heap
empty = Empty

singleton :: Int -> Heap
singleton n =
  Node 1 n empty empty
{-# INLINE singleton #-}

size :: Heap -> Int
size (Node s _ _ _) = s
size _ = 0

insert :: Int -> Heap -> Heap
insert n =
  merge (singleton n)

view :: Heap -> Maybe (Int, Heap)
view (Node _size n l r) = Just (n, merge l r)
view _ = Nothing

viewHead :: Heap -> Maybe Int
viewHead (Node _ n _ _) = Just n
viewHead _ = Nothing

deleteMin :: Heap -> Heap
deleteMin (Node _ _ l r) =
  merge l r
deleteMin _ =
  error "empty"

merge :: Heap -> Heap -> Heap
merge Empty h = h
merge h Empty = h
merge h@(Node s1 n l r) g@(Node s2 m l' r')
  | n < m     = Node (s1+s2) n (merge g l) r
  | otherwise = Node (s1+s2) m (merge h l') r'

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
