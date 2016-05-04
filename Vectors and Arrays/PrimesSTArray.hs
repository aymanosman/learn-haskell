module Main where

import System.Environment (getArgs)
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Array.ST
import Data.Array.Unboxed

sieveUA :: Int -> UArray Int Bool
sieveUA top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True      -- :: ST s (STUArray s Int Bool)
    forM_ [1..r `div` 2] $ \i -> do
      isPrime <- readArray sieve i
      when isPrime $               -- ((2*i+1)^2-1)`div`2 == 2*i*(i+1)
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j ->
          writeArray sieve j False
    return sieve

primesToUA :: Int -> [Int]
primesToUA top = 2 : [i*2+1 | (i,True) <- assocs $ sieveUA top]

primesTo m =
  sieve [2..m]       {- (\\) is set-difference for unordered lists -}
  where
    sieve (x:xs) = x : sieve (xs Data.List.\\ [x,x+x..m])
    sieve [] = []

main = do
  [n'] <- getArgs
  print $ length $ primesToUA (read n')
