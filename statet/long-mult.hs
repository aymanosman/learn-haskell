module Main where

import Control.Monad (guard, mfilter, replicateM)
import Control.Monad.Trans.State
import Data.List (find, delete, foldl', union, permutations)

-- |
--     A B C
-- x     3 D
-- ---------
--   E F G H
--
main :: IO ()
main =
  print shortAnswer

digits = delete 3 [1..9]

shortAnswer =
  find (\(abc,_3d,efgh) -> abc * _3d == efgh)
  $ map convertDigits
  $ permutations digits

convertDigits [a,b,c,d,e,f,g,h] =
  ( unDigits [a,b,c]
  , unDigits [3,d]
  , unDigits [e,f,g,h]
  )

unDigits :: [Int] -> Int
unDigits = foldl' (\a b -> a*10 + b) 0






-- Long Answer


run =
  evalStateT app digits

app :: StateT [Int] [] (Int, Int, Int)
app =
  do digits <- replicateM 8 $ StateT select
     let (abc, _3d, efgh) = convertDigits digits
     guard $ abc * _3d == efgh
     return (abc, _3d, efgh)

-- Attempt to find constraints that reduce the number of possible candidates
-- So far reduction is 40,320 (8!) -> 28,800
app2 =
  do [a,b,c,d,e,f,g,h] <- replicateM 8 $ StateT select
     let (abc, _3d, efgh) = convertDigits [a,b,c,d,e,f,g,h]
     guard $ [7,9] `union` [c,d] /= [7,9] -- really mean some kind of set comparison
     guard $ c /= 1 && d /= 1
     return (abc, _3d, efgh)


select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]
