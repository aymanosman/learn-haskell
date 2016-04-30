module Main where

import qualified Data.List as List
import Debug.Trace (trace)
import qualified Data.Heap as H
import Test.QuickCheck  -- (quickCheck)

type Heap = H.MinHeap Int

data Ans =
  Done Heap
  | More Heap
  | Fail

main :: IO ()
main =
  do let readInts = fmap read `fmap` words `fmap` getLine :: IO [Int]
     [_, sweetness] <- readInts
     cs <- readInts
     -- assert length cs == _ above
     print $ ans sweetness cs

ans :: Int -> [Int] -> Int
ans m l =
  case ans' 0 m (H.fromList l) of
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

step :: Int -> H.MinHeap Int -> Ans
step m heap
  | H.size heap < 2 =
    case H.viewHead heap of
      Nothing -> Fail
      Just n ->
        if n >= m
        then Done heap
        else Fail
  | otherwise =
    let Just (a, h) = H.view heap
        Just (b, h') = H.view h
    in
      if a >= m && b >= m
      then Done heap
      else More $ H.insert (a+2*b) h'

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

main' :: IO ()
main' =
  do putStrLn "==="
     print $ ans (10^7) [1,2,3,9,10,12]
     print $ ans 0 [1,2]
     putStrLn "=== QuickCheck ==="
     quickCheck prop_same_as_naive

prop_same_as_naive :: NonNegative Int -> NonEmptyList (Positive Int) -> Bool
prop_same_as_naive m' l' =
  let
    m = getNonNegative m'
    l = map getPositive (getNonEmpty l')
    a = ans m l
    b = naive 0 m (List.sort l)
  in
  -- trace ("ans: " ++ show a)
  a
  ==
  -- trace ("naive: " ++ show b)
  b
