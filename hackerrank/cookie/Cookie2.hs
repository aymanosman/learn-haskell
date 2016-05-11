module Cookie2 (ans) where

import BinaryHeap

data Ans a =
  Done (BinaryHeap a)
  | More (BinaryHeap a)
  | Fail (BinaryHeap a)

ans :: (Integral a)
  => a -> [a] -> (a, [a])
ans m l =
  case ans' 0 m (fromList l) of
    Left (_, h) -> (-1, toList h)
    Right (n, h) -> (n, toList h)

ans' :: (Integral a)
  => a -> a -> BinaryHeap a -> Either (a, BinaryHeap a) (a, BinaryHeap a)
ans' n sweetness h =
  case step sweetness h of
    Fail heap ->
      Left (n, heap)

    Done heap ->
      Right (n, heap)

    More heap ->
      n `seq` heap `seq` ans' (n+1) sweetness heap

step :: Integral a
  => a -> BinaryHeap a -> Ans a
step m heap
  | size heap < 2 =
    case findMin heap of
      Nothing ->
        Fail heap

      Just n ->
        if n >= m
        then Done heap
        else Fail heap
  | otherwise =
    let Just (a, h) = view heap
        Just (b, h') = view h
    in
      if a >= m && b >= m
      then Done heap
      else More $ a `seq` b `seq` h' `seq` insert (a+2*b) h'


-- main :: IO ()
-- main = program

-- program =
--   do let readInts = fmap read `fmap` words `fmap` getLine :: IO [Int]
--      [_, sweetness] <- readInts
--      cs <- readInts
--      -- assert length cs == _ above
--      print $ ans sweetness cs
