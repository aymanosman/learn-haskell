module Main where

import System.Random
import Control.Monad

import qualified Data.List as List
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.MArray.Heapsort (sort)

size = 1000000

main :: IO ()
main = do
  xs <- replicateM size $ randomRIO (1,100) :: IO [Int]
  -- print (List.sort xs)
  let arr2 = gg xs
  print (elems arr2)

gg :: [Int] -> UArray Int Int
gg xs = runSTUArray $ do
  arr <- newListArray (1,size) xs
  sort arr
  return arr


