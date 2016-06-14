{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment (getArgs)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, elems)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray (IArray, indices, (!))
import Data.Array.ST (STArray, Ix)
import qualified Data.Array.ST as Array
import Data.Array.ST as Array

ff :: Array Int Int
ff = runSTArray $ do
  arr <- newArray_ (1,2)
  let (#) = writeArray arr
  1#23
  2#42
  return arr

go xs = do
  arr <- newListArray (1,3) xs :: ST s (STArray s Int Int)
  a <- readArray arr 1
  siftDown arr 1 6
  b <- readArray arr 1
  return arr

type MyArr s = STArray s Int Int

-- [3,1,2]
siftDown :: MyArr s -> Int -> Int -> ST s (MyArr s)
siftDown arr i end = do
  -- if left < end && right < end then
  --   left <- readArray arr child
  swap arr i (2*i)
  -- a <- writeArray arr i 23
  return arr

swap arr a b = do
  a' <- readArray arr a
  b' <- readArray arr b
  writeArray arr a b'
  writeArray arr b a'
  return arr


main :: IO ()
main = do
  let xs = [3,1,2]
  print xs
  print $ elems $ runSTArray $ go xs -- [1,2,3,9,11,12]
-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [which, x] ->  program which x
--     _ -> usage

usage = putStrLn "Usage: [slow|fast] num"

program which x =
  case which of
    "slow" ->
      let arr = mkArr $ read x
      in print $ foldASlow (+) 0 arr
    "fast" ->
      let arr = mkArr $ read x
      in print $ foldA (+) 0 arr
    _ -> usage

mkArr :: Int -> UArray Int Int
mkArr x = Array.runSTUArray $
  Array.newListArray (1,x) [1..x]

foldA :: (Ix i, IArray a e) => (t -> e -> t) -> t -> a i e -> t
foldA f s' a =
  go s' (indices a)
  where
    go !s (j:js) =
      go (f s (a!j)) js
    go s _ = s

foldASlow :: (Ix i, IArray a e) => (t -> e -> t) -> t -> a i e -> t
foldASlow f s' a =
  go s' (indices a)
  where
    go s (j:js) =
      go (f s (a!j)) js
    go s _ = s
