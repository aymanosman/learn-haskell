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
import Control.DeepSeq (NFData)

time :: String -> IO () -> IO ()
time s act =
  do (m, _) <- measure (nfIO act) 1
     putStr (s++": ")
     putStrLn $ secs $ measTime m

maxNum = 1000000 -- 1 million

main = do
  [which, n'] <- getArgs
  let n = read n'
  case which of
    "array" ->
      time "array" $ runArray n
    "list" ->
      time "list" $ runList n
    "array2" ->
      time "array2" $ runArray2 n
    "all" -> do
      time "list" $ runList n
      time "array" $ runArray n
      time "array2" $ runArray2 n


runArray size = do
  xs <- replicateM size (randomRIO (1,maxNum)) :: IO [Int]
  arr <- A.newListArray (1,size) xs :: IO (A.IOArray Int Int)
  -- swap (1,size) arr
  heapsort arr
  -- print =<< A.getElems arr
  print =<< A.readArray arr 1
  -- print =<< A.readArray arr size

runArray2 size = do
  xs <- replicateM size (randomRIO (1,maxNum)) :: IO [Int]
  arr <- A.newListArray (1,size) xs :: IO (A.IOArray Int Int)
  -- swap (1,size) arr
  sort arr
  -- print =<< A.getElems arr
  print =<< A.readArray arr 1
  -- print =<< A.readArray arr size

runList size = do
  xs <- replicateM size (randomRIO (1,maxNum)) :: IO [Int]
  let l = List.sort xs
  print $ head l

swap :: (Int, Int) -> A.IOArray Int Int -> IO ()
swap (n,m) arr = do
  x <- A.readArray arr n
  y <- A.readArray arr m
  A.writeArray arr m x
  A.writeArray arr n y



data NodeType = Inner | Leaf | Edge

{-# INLINE heapsort #-}
heapsort :: (Integral i, Ix i, Ord e, MArray a e m) => a i e -> m ()
-- | Sort the elements of the given 'MArray' in increasing order.
heapsort a = getBounds a >>= uncurry (heapsort' a)

{-# INLINE heapsort' #-}
heapsort' :: (Integral i, Ix i, Ord e, MArray a e m) => a i e -> i -> i -> m ()
-- | Sort the elements of the given 'MArray' in increasing order.
heapsort' a mn mx = do
    heapify
    extract
  where
    getN = mx - mn + 1

    {-# INLINE right #-}
    right i = 2 * i + 2

    {-# INLINE nodeType #-}
    nodeType n i = case compare (right i) n of
        GT  -> Leaf
        EQ  -> Edge
        LT  -> Inner

    {-# INLINE atIndex #-}
    atIndex i = readArray a (i + mn)

    {-# INLINE atIndexW #-}
    atIndexW i = writeArray a (i + mn)

    {-# INLINE downHeap #-}
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
                {-# INLINE stop #-}
                stop = when (i /= j) (atIndexW i c)
                ir = right i
                il = ir - 1

    {-# INLINE heapify #-}
    heapify = heapifyRoots (n - 1) -- optimize: use 2^k such that k is maximal that 2^k < n
      where
        n = getN
        heapifyRoots i | i < 0  = return ()
                       | otherwise = downHeap n i >> heapifyRoots (i - 1)


    {-# INLINE exch #-}
    exch ix1 ix2 | ix1 == ix2   = return ()
                 | otherwise    = do
      let i = ix1 + mn
          j = ix2 + mn
      v1 <- readArray a i
      v2 <- readArray a j
      writeArray a j v1
      writeArray a i v2

    {-# INLINE extract #-}
    extract = extractRoot (getN - 1)
      where
        extractRoot k
          | k <= 0    = return ()
          | otherwise = do
              exch k 0
              downHeap k 0
              extractRoot (k - 1)
