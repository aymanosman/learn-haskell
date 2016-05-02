module Main where

import System.Random
import qualified System.Random.Mersenne as M
import System.Environment
import qualified Data.Judy as J
import Control.Monad
import qualified Data.Heap as H

import Data.Binary
import qualified Data.ByteString.Lazy as BS

import qualified Data.PQueue.Max as PQ

-- TODO: compare to heap and stuff
n = 1000000

pqueue = do
  s <- BS.readFile "sample.txt"
  let rs = decode s :: [Word]
  let pq = PQ.fromList rs
  let Just (v, _) = PQ.maxView pq
  print v

main = do
  [which] <- getArgs
  case which of
    "judy" -> judy
    "judy-direct" -> judyDirect
    "heap" -> heap
    "pqueue" -> pqueue
    "sample" -> writeNums
    _ -> error "failed"

judy = do
   s <- BS.readFile "sample.txt"
   let rs = decode s :: [Word]
   j  <- J.new :: IO (J.JudyL Int)
   forM_ rs $ \n ->
       J.insert n 1 j
   Just (v, _)  <- J.findMax j
   print v

judyDirect = do
   g <- M.getStdGen
   rs <- M.randoms g
   j  <- J.new :: IO (J.JudyL Int)
   forM_ (take n rs) $ \n ->
       J.insert n 1 j
   Just (v, _)  <- J.findMax j
   print v


heap = do
   s <- BS.readFile "sample.txt"
   let rs = decode s :: [Word]
   let h = H.fromList rs :: H.MaxHeap Word
   let Just w = H.viewHead h
   print w

writeNums = do
   g  <- getStdGen
   let rs = randoms g :: [Word]
   BS.writeFile "sample.txt" $ encode (take n rs)



