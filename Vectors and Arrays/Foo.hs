module Main where

import System.Environment (getArgs)
import System.Random
import Control.Monad

import qualified Data.List as List
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.MArray.Heapsort (sort)

-- size = 1000000

main :: IO ()
main =
  run 1000
  -- program

program = do
  [n'] <- getArgs
  let size = read n' :: Int
  run size

run size = do
  xs <- replicateM size $ randomRIO (1,100) :: IO [Int]
  -- print $ length $ (List.sort xs)
  let arr2 = gg size xs
  print $ arr2 ! (size`div`2)

gg :: Int -> [Int] -> UArray Int Int
gg size xs = runSTUArray $ do
  arr <- newListArray (1,size) xs
  sort arr
  return arr


