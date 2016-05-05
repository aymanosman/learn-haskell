module Cookie3 (ans) where

import qualified Data.List as List

data Ans a =
  Done [a]
  | More [a]
  | Fail [a]

ans :: (Integral a)
  => a -> [a] -> (a, [a])
ans m l =
  case ans' 0 m l of
    Left (_, h) -> (-1, h)
    Right (n, h) -> (n, h)

ans' :: (Integral a)
  => a -> a -> [a] -> Either (a, [a]) (a, [a])
ans' n sweetness h =
  case step sweetness h of
    Fail l ->
      Left (n, l)

    Done l ->
      Right (n, l)

    More l ->
      ans' (n+1) sweetness l

step :: Integral a
  => a -> [a] -> Ans a
step m l
  | length l < 2 =
    case l of
      [] ->
        Fail l

      [n] ->
        if n >= m
        then Done l
        else Fail l
  | otherwise =
    let (a:h) = l
        (b:h') = h
    in
      if a >= m && b >= m
      then Done l
      else More $ List.sort $ (a+2*b):h'


-- main :: IO ()
-- main = program

-- program =
--   do let readInts = fmap read `fmap` words `fmap` getLine :: IO [Int]
--      [_, sweetness] <- readInts
--      cs <- readInts
--      -- assert length cs == _ above
--      print $ ans sweetness cs
