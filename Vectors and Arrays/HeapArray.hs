module Main where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import qualified Data.List as List
import qualified Data.Array.IO as A

main = do
  let size = 1000000
  xs <- replicateM size (randomRIO (1,100)) :: IO [Int]
  let l = List.sort xs
  print (take 10 l)
  arr <- A.newListArray (1,size) xs
  -- print =<< A.getElems arr
  swap (1,size) arr
  -- print =<< A.getElems arr

swap :: (Int, Int) -> A.IOArray Int Int -> IO ()
swap (n,m) arr = do
  x <- A.readArray arr n
  y <- A.readArray arr m
  A.writeArray arr m x
  A.writeArray arr n y

heapsort :: (Int, Int) -> A.IOArray Int Int -> IO ()
heapsort =
  undefined
