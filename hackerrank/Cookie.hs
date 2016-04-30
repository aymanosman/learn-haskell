module Cookie where

import qualified Data.List as List
import qualified Data.Heap as H

main' :: IO ()
main' =
  do let readInts = fmap read `fmap` words `fmap` getLine :: IO [Int]
     [_, sweetness] <- readInts
     cs <- readInts
     -- assert length cs == _ above
     putStrLn $ toString (ans sweetness cs)

toString :: Show a => Maybe (a, t) -> String
toString Nothing = "-1"
toString (Just (n, _)) = show n

ans :: Int -> [Int] -> Maybe (Int, [Int])
ans m l =
  case ans' 0 m (H.fromList l) of
    Nothing -> Nothing
    Just (t, heap) ->
      Just (t, H.toList heap)

ans' :: Int -> Int -> Heap -> Maybe (Int, Heap)
ans' n sweetness h =
  case step sweetness h of
    Fail -> Nothing

    Done heap ->
      let Just a = H.viewHead heap
      in
        Just (a, heap)

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
       x = ans 7 l
       y = ans 7 (List.sort l)
     print x
     print y
     putStrLn $ toString x
     putStrLn $ toString y

