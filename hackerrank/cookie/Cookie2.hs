module Cookie2 (ans) where

import BinaryHeap

data Ans a =
  Done (BinaryHeap a)
  | More (BinaryHeap a)
  | Fail

-- main :: IO ()
-- main = program

-- program =
--   do let readInts = fmap read `fmap` words `fmap` getLine :: IO [Int]
--      [_, sweetness] <- readInts
--      cs <- readInts
--      -- assert length cs == _ above
--      print $ ans sweetness cs

ans :: (Integral a)
  => a -> [a] -> a
ans m l =
  case ans' 0 m (fromList l) of
    Nothing -> -1
    Just (n, _) -> n

ans' :: Int -> Int -> BinaryHeap a -> Maybe (Int, BinaryHeap a)
ans' n sweetness h =
  case step sweetness h of
    Fail -> Nothing

    Done heap ->
      Just (n, heap)

    More heap ->
      ans' (n+1) sweetness heap

step :: Int -> BinaryHeap a -> Ans a
step m heap
  | size heap < 2 =
    case findMin heap of
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

