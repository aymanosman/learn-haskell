module Cookie where

import qualified Data.List as List

main' :: IO ()
main' =
  do let readInts = fmap read `fmap` words `fmap` getLine :: IO [Int]
     [_, sweetness] <- readInts
     cs <- readInts
     -- make heap?
     -- assert length cs == _ above
     putStrLn $ toString (ans sweetness (List.sort cs))

toString :: Show a => Maybe (a, t) -> String
toString Nothing = "-1"
toString (Just (n, _)) = show n

--ans2 :: Int -> MinHeap Int -> ...
ans :: Int -> [Int] -> Maybe (Int, [Int])
ans =
  ans' 0

ans' :: (Num a, Num t, Ord a) => t -> a -> [a] -> Maybe (t, [a])
ans' n sweetness l
  | all (>=sweetness) l = Just (n, l)
  | otherwise =
     case l of
       a:b:xs ->
         ans' (n+1) sweetness $ (a+2*b):xs
       _ ->
         Nothing

--
main :: IO ()
main =
  do let x = ans 7 [1, 2, 3, 9, 10, 12]
     print x
     putStrLn $ toString x

