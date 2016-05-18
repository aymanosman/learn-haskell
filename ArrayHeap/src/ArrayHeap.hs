{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment (getArgs)
import Control.Monad.ST (ST)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray (elems, indices, (!))
import Data.Array.ST (STArray, Ix)
import qualified Data.Array.ST as Array

main :: IO ()
main = do
  [which, x] <- getArgs
  case which of
    "slow" ->
      let arr = mkArr_slow $ read x
      in print $ foldA_slow (+) 0 arr
    "fast" ->
      let arr = mkArr $ read x
      in print $ foldA (+) 0 arr


asd ::(Ix i)
  => (forall s. ST s (STArray s i e)) -> Array i e
asd = Array.runSTArray

mkArr :: Int -> UArray Int Int
mkArr x = Array.runSTUArray $ do
  arr <- Array.newListArray (1,x) [1..x]
  -- Array.writeArray arr 1 (42::Int)
  return arr

mkArr_slow :: Int -> UArray Int Int
mkArr_slow x = Array.runSTUArray $ do
  -- let xs = [1..x]
  -- arr <- Array.newListArray (1,length xs) xs
  arr <- Array.newListArray (1,x) [1..x]
  -- Array.writeArray arr 1 (42::Int)
  return arr

foldA f s a =
  go s (indices a)
  where
    go !s (j:js) =
      go (f s (a!j)) js
    go s _ = s

foldA_slow f s a =
  go s (indices a)
  where
    go s (j:js) =
      go (f s (a!j)) js
    go s _ = s
