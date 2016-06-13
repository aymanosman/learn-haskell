{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment (getArgs)
import Control.Monad.ST (ST)
import Data.Array (Array)
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
  args <- getArgs
  case args of
    [which, x] ->  program which x
    _ -> usage

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
