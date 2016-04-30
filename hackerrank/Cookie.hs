module Cookie where

import qualified Data.List as List
import Debug.Trace (trace)
import qualified Data.Heap as H
import Test.QuickCheck (quickCheck)

main' :: IO ()
main' =
  do let readInts = fmap read `fmap` words `fmap` getLine :: IO [Int]
     [_, sweetness] <- readInts
     cs <- readInts
     -- assert length cs == _ above
     print $ ans sweetness cs

ans :: Int -> [Int] -> Int
ans m l =
  case ans' 0 m (H.fromList l) of
    Nothing -> -1
    Just (t, _) -> t

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

naive :: Int -> Int -> [Int] -> Int
naive n m l
  | not (null l) && all (>m) l = -- expect sorted list
    n
  | otherwise =
    case l of
      a:b:xs ->
        let l' = List.sort ((a+2*b):xs)
        in
        naive (n+1) m
        -- $ trace ("l: " ++ show l') l'
        $ l'
      _ ->
        -1

type Heap = H.MinHeap Int
data Ans =
  Done Heap
  | More Heap
  | Fail
--
main :: IO ()
main =
  do let
       l = [12, 10, 9, 3, 2, 1]
       h = H.fromList l
       x = ans' 0 7 (H.fromList l)
       y = naive 0 7 (List.sort l)
       -- y = ansNaive 0 7 (List.sort l)
     print x
     print y
     putStrLn "==="
     print $ ans' 0 10 h
     print $ ans' 0 20 h
     print $ ans' 0 100 h
     print $ ans' 0 110 h
     putStrLn "=== QuickCheck ==="
     quickCheck prop_same_as_naive


-- tests
{-
 failed: [-5, 4, 2]
 -}
prop_same_as_naive :: [Int] -> Bool
prop_same_as_naive l =
  let
    a = ans 7 l
    b = naive 0 7 (List.sort l)

  in
  -- trace ("ans: " ++ show a) a
  a
  ==
  b
  -- trace ("naive: " ++ show b) b
