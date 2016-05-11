module Main where

import System.Environment (getArgs)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import qualified Data.List as List
import Data.Array (Array, elems)
import Data.Array.ST -- (runSTArray, MArray, newListArray, writeArray, readArray)

main = do
  let size = 10--000
  xs <- replicateM size (randomRIO (1,100)) :: IO [Int]
  let l = List.sort xs
  print (take 10 l)
  print (take 10 xs)
  let arr = run size xs :: Array Int Int
  print $ take 10 $ elems arr
  print 42

-- run :: Int -> [Int] -> Array Int Int
run size xs = runSTArray $ do
  arr <- newListArray (1,size) xs
  -- print =<< getElems arr
  swap 1 size arr
  -- print =<< getElems arr
  return arr

swap :: (MArray a e m)
 =>  Int -> Int -> a Int e-> m ()
swap n m arr = do
  x <- readArray arr n
  y <- readArray arr m
  writeArray arr m x
  writeArray arr n y

-- heapsort :: (Int, Int) -> A.IOArray Int Int -> IO ()
-- heapsort =
--   undefined
