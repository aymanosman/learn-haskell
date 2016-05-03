module Main where

import System.Random
import System.Environment
import System.IO (hGetLine, openFile, IOMode(ReadMode), Handle)
import Control.Monad
import Criterion.Main
import Criterion.Measurement
import Criterion.Types (Measured(..))
import qualified Data.Attoparsec.Text as P

import qualified Data.Heap as H

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import qualified Data.PQueue.Max as PQ

import Heap
import qualified BinaryHeap as BinaryHeap

-- TODO: compare to heap and stuff
binaryFile = "binary.txt"
textFile = "text.txt"
n = 1000000
maxSweetness = 1000000000 -- billion

-- time :: IO a -> IO ()
time s act =
  do (m, d) <- measure (nfIO act) 1
     putStr (s++": ")
     putStrLn $ secs $ measTime m

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) . decodeUtf8 <$> B.hGetLine h

run :: Handle -> String -> ([Word] -> IO ()) -> IO ()
run h s f =
  do print 42
     ret1 <- readWords h
     ret2 <- readWords h
     either error (\[n, m] ->
       either error (time s . f) ret2) ret1

binaryHeap cs = do
  let h = BinaryHeap.fromList cs
  -- TODO: fix up heap api
  let Just v = findMin h
  print v

heap cs = do
  let h = H.fromList cs :: H.MaxHeap Word
  let Just w = H.viewHead h
  print w

pqueue cs = do
  let pq = PQ.fromList cs
  let Just (v, _) = PQ.maxView pq
  print v

writeBinary = do
  g  <- getStdGen
  let rs = randomRs (0, maxSweetness) g :: [Word]
  BS.writeFile binaryFile $ encode (take n rs)

writeText = do
  g  <- getStdGen
  let rs = randomRs (0, maxSweetness) g :: [Word]
  let s =
        unlines
        [ unwords ["1000000", show (maxSweetness `div` 2 :: Word)]
        , unwords $ map show (take n rs)
        ]
  writeFile textFile s



program = do
  h <- openFile "text.txt" ReadMode
  [which] <- getArgs
  case which of
    "heap" ->
      run h "heap" heap

    "binary-heap" ->
      run h "binary-heap" binaryHeap

    "pqueue" ->
      run h "pqueue" pqueue

    "binary" -> writeBinary
    "text" -> writeText
    _ -> error "failed"

main :: IO ()
main = test

test =
 do h <- openFile "text.txt" ReadMode
    run h "pqueue" pqueue
