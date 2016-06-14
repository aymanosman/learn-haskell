{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace (trace)
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

main :: IO ()
main = do
  let xs = [12,11,9,3,2,1]
  print xs
  print $ elems $ runSTArray $ go xs -- [1,2,3,9,11,12]

go xs = do
  let end = length xs
  arr <- newListArray (1,end) xs :: ST s (STArray s Int Int)
  a <- readArray arr 1
  siftDown arr 1 end
  b <- readArray arr 1
  return arr

type MyArr s = STArray s Int Int

-- [3,1,2]
siftDown :: MyArr s -> Int -> Int -> ST s (MyArr s)
siftDown arr i end = do
  -- if left < end && right < end then
  --   left <- readArray arr child
  val <- readArray arr i
  if left <= end then do
    a <- trace ("left: " ++ show left) $ swap arr left val
    if a == val then
      trace ("a: " ++ show a)
      $ trace ("val: " ++ show val)
      $ return arr
    else do
      -- fill hole
      writeArray arr i a
      if right <= end then do
        b <- trace ("right: " ++ show right) $ swap arr right val
        writeArray arr left b
        if b == val then
          trace ("a: " ++ show a)
          $ trace ("b: " ++ show b)
          $ trace ("val: " ++ show val)
          $ return arr
        else
          trace ("a: " ++ show a)
          $ trace ("b: " ++ show b)
          $ trace ("val: " ++ show val)
          $ siftDown arr right end
      else
        return arr
  else
    return arr
  where
    left = 2*i
    right = left + 1

swap arr pos val = do
  a <- readArray arr pos
  if a < val then do
    writeArray arr pos val
    return a
  else
    return val

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
