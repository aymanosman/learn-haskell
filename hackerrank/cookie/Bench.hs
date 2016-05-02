module Main where

import System.Random
import qualified System.Random.Mersenne as M
import System.Environment
import qualified Data.Judy as J
import Control.Monad
import Control.DeepSeq
import qualified Data.Heap as H

import Data.Binary
import qualified Data.ByteString.Lazy as BS

import qualified Data.PQueue.Max as PQ

-- TODO: compare to heap and stuff
binaryFile = "binary.txt"
textFile = "text.txt"
n = 100--0000

pqueue = do
  s <- BS.readFile binaryFile
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
    "binary" -> writeBinary
    "text" -> writeText
    _ -> error "failed"

judy = do
  s <- BS.readFile binaryFile
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
  s <- BS.readFile binaryFile
  let rs = decode s :: [Word]
  let h = H.fromList rs :: H.MaxHeap Word
  let Just w = H.viewHead h
  print w

writeBinary = do
  g  <- getStdGen
  let rs = randoms g :: [Word]
  BS.writeFile binaryFile $ encode (take n rs)

writeText = do
  g  <- getStdGen
  let rs = randoms g :: [Word]
  let s = "1000000 42424242\n" ++ (unwords $ map show (take n rs))
  writeFile textFile s




