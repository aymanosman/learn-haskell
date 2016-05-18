module Main where
-- module Heapsort (heapsort, heapsort') where


import System.Random (randomRIO)
import System.Environment (getArgs)
import Control.Monad (replicateM, when)
import qualified Data.List as List

import Data.Array.MArray
import Data.Array.MArray.Heapsort (sort)
import qualified Data.Array.IO as A

import Criterion.Main (nfIO)
import Criterion.Measurement (measure, secs)
import Criterion.Types (Measured(..))

time :: String -> IO () -> IO ()
time s act =
  do (m, _) <- measure (nfIO act) 1
     putStr (s++": ")
     putStrLn $ secs $ measTime m

main = do
  args <- getArgs
  case args of
    [which, n'] -> do
      let size = read n'
          maxNum = 1000000 -- 1 million
      xs <- replicateM size (randomRIO (1,maxNum)) :: IO [Int]
      case which of
        "array" ->
          time "array" $ runArray size xs
        "list" ->
          time "list" $ runList xs
        "array2" ->
          time "array2" $ runArray2 size xs
        "all" -> do
          time "list" $ runList xs
          time "array" $ runArray size xs
          time "array2" $ runArray2 size xs

    _ -> usage

usage = do
  putStrLn "Usage: <which> <num>"
  putStrLn "    which: array list array2 all"
  putStrLn "    num: N number of elements to insert"

runArray size xs = do
  arr <- A.newListArray (1,size) xs :: IO (A.IOArray Int Int)
  heapsort arr
  print =<< A.readArray arr 1

runArray2 size xs = do
  arr <- A.newListArray (1,size) xs :: IO (A.IOArray Int Int)
  sort arr
  print =<< A.readArray arr 1

runList xs = do
  let l = List.sort xs
  print $ head l

swap :: (Int, Int) -> A.IOArray Int Int -> IO ()
swap (n,m) arr = do
  x <- A.readArray arr n
  y <- A.readArray arr m
  A.writeArray arr m x
  A.writeArray arr n y



data NodeType = Inner | Leaf | Edge

heapsort :: (Integral i, Ix i, Ord e, MArray a e m) => a i e -> m ()
-- | Sort the elements of the given 'MArray' in increasing order.
heapsort a = getBounds a >>= uncurry (heapsort' a)

heapsort' :: (Integral i, Ix i, Ord e, MArray a e m) => a i e -> i -> i -> m ()
-- | Sort the elements of the given 'MArray' in increasing order.
heapsort' a mn mx = do
    heapify
    extract
  where
    getN = mx - mn + 1

    right i = 2 * i + 2

    nodeType n i = case compare (right i) n of
        GT  -> Leaf
        EQ  -> Edge
        LT  -> Inner

    atIndex i = readArray a (i + mn)

    atIndexW i = writeArray a (i + mn)

    downHeap n j = do
        c <- atIndex j
        dh c j
      where
        -- Propagate @c@ down the heap. We only move elements up as we walk on
        -- the nodes, and when we're finished, we write @c@ to the last node.
        -- This way we save half of read/writes, compared to swapping elements.
        dh c = dhStep
          where
            dhStep i = case nodeType n i of
                Leaf -> stop
                Edge -> do
                    l <- atIndex il
                    if c < l
                      then atIndexW i l >> atIndexW il c
                      else stop
                Inner -> do
                    l <- atIndex il
                    r <- atIndex ir
                    if c >= l && c >= r
                      then stop
                      else if l >= r
                        then atIndexW i l >> dhStep il
                        else atIndexW i r >> dhStep ir
              where
                stop = when (i /= j) (atIndexW i c)
                ir = right i
                il = ir - 1

    heapify = heapifyRoots (n - 1) -- optimize: use 2^k such that k is maximal that 2^k < n
      where
        n = getN
        heapifyRoots i | i < 0  = return ()
                       | otherwise = downHeap n i >> heapifyRoots (i - 1)


    exch ix1 ix2 | ix1 == ix2   = return ()
                 | otherwise    = do
      let i = ix1 + mn
          j = ix2 + mn
      v1 <- readArray a i
      v2 <- readArray a j
      writeArray a j v1
      writeArray a i v2

    extract = extractRoot (getN - 1)
      where
        extractRoot k
          | k <= 0    = return ()
          | otherwise = do
              exch k 0
              downHeap k 0
              extractRoot (k - 1)
