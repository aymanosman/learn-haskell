import Data.List
import Test.QuickCheck

main :: IO ()
main =
  do print tests
     check

tests =
  [ dig 98 == [9, 8]
  , dig 829 == [8, 2, 9]
  ]

check =
  quickCheck prop_roundtrip

prop_roundtrip n =
  n == unDigits (dig n)

dig :: Int -> [Int]
dig n | n < 10 = [n]
dig n =
  let (d,b) = head (g n)
  in
  d:dig (n - (d*b))

g n =
  reverse $ unfoldr (f n) 10


f :: Int -> Int -> Maybe ((Int, Int), Int)
f n m =
  let x = n `div` m
  in
  if x == 0
    then Nothing
    else Just ((x, m), m*10)


unDigits :: [Int] -> Int
unDigits = foldl' (\a b -> a*10 + b) 0
