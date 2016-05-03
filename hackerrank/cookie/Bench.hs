module Main where

import System.Random
import qualified System.Random.Mersenne as M
import System.Environment
import qualified Data.Judy as J
import Control.Monad
import Criterion.Main
import Criterion.Measurement
import Criterion.Types (Measured(..))

import qualified Data.Heap as H

import Data.Binary
import qualified Data.ByteString.Lazy as BS

import qualified Data.PQueue.Max as PQ

import Heap
import qualified BinaryHeap as BinaryHeap

-- TODO: compare to heap and stuff
binaryFile = "binary.txt"
textFile = "text.txt"
n = 1000000

-- time :: IO a -> IO ()
time s act =
  do (m, d) <- measure (nfIO act) 1
     putStr (s++": ")
     putStrLn $ secs $ measTime m

main = do
  [which] <- getArgs
  case which of
    "judy" ->
      time "judy" judy

    "judy-direct" ->
      time "judy-direct" judyDirect

    "heap" ->
      time "heap" heap

    "binary-heap" ->
      time "binary-heap" binaryHeap

    "pqueue" ->
      time "pqueue" pqueue

    "binary" -> writeBinary
    "text" -> writeText
    _ -> error "failed"

main' = do
  [which] <- getArgs
  case which of
    "judy" -> judy
    "judy-direct" -> judyDirect
    "heap" -> heap
    "binary-heap" -> binaryHeap
    "pqueue" -> pqueue
    "binary" -> writeBinary
    "text" -> writeText
    _ -> error "failed"

binaryHeap = do
  s <- BS.readFile binaryFile
  let rs = decode s :: [Word]
  let h = BinaryHeap.fromList rs :: BinaryHeap.BinaryHeap Word
  -- TODO: fix up heap api
  let Just v = findMin h
  print v

heap = do
  s <- BS.readFile binaryFile
  let rs = decode s :: [Word]
  let h = H.fromList rs :: H.MaxHeap Word
  let Just w = H.viewHead h
  print w

pqueue = do
  s <- BS.readFile binaryFile
  let rs = decode s :: [Word]
  let pq = PQ.fromList rs
  let Just (v, _) = PQ.maxView pq
  print v

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


writeBinary = do
  g  <- getStdGen
  let rs = randoms g :: [Word]
  BS.writeFile binaryFile $ encode (take n rs)

writeText = do
  g  <- getStdGen
  let rs = randoms g :: [Word]
  let s =
        unlines
        [ unwords ["1000000", show (maxBound `div` 2 :: Word)]
        , unwords $ map show (take n rs)
        ]
  writeFile textFile s




