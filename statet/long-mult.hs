module Main where

import Control.Monad             (guard, mfilter, replicateM)
import Control.Monad.Trans.State
import Data.List                 (foldl')

-- |
--     A B C
-- x     3 D
-- ---------
--   E F G H
--
main :: IO ()
main =
  do let r = evalStateT app [1, 2, 4, 5, 6, 7, 8, 9]
     print r

app :: StateT [Int] [] (Int, Int, Int)
app =
  do [a,b,c,d,e,f,g,h] <- replicateM 8 $ StateT select
     let abc = asNumber [a,b,c]
         _3d = asNumber [3,d]
         efgh = asNumber [e,f,g,h]
     guard $ abc * _3d  == efgh
     return (abc, _3d, efgh)


select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

asNumber :: [Int] -> Int
asNumber = foldl' (\t o -> t*10 + o) 0
