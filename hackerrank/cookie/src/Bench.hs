module Main where

import System.Random
import System.Environment

import System.IO (openFile, IOMode(ReadMode), Handle)
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text as P

import qualified Data.Heap as H
import qualified Data.PQueue.Max as PQ
import Heap
import qualified BinaryHeap

import Criterion.Main
import Criterion.Measurement
import Criterion.Types (Measured(..))
import Control.DeepSeq (NFData)

binaryFile, textFile :: String
binaryFile = "binary.txt"
textFile = "text.txt"

main, program :: IO ()
main = program

time :: (NFData a)
  => String -> IO a -> IO ()
time s act =
  do (m, _) <- measure (nfIO act) 1
     putStr (s++": ")
     putStrLn $ secs $ measTime m

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) . decodeUtf8
  <$> B.hGetLine h

run :: Handle -> String -> ([Word] -> IO ()) -> IO ()
run h s f =
  do
     Right [n, m] <- readWords h
     Right cs <- readWords h
     time s (f cs)


heap, binaryHeap, pqueue :: [Word] -> IO ()
-- Numbers are for insertion of 1million elements and finding the max
-- binary-heap: 247.1ms
binaryHeap cs = do
  let h = BinaryHeap.fromList cs
  -- TODO: fix up heap api
  let Just v = findMin h
  print v

-- heap: 4.856 s -- why so slow??
heap cs = do
  let h = H.fromList cs :: H.MaxHeap Word
  let Just w = H.viewHead h
  print w

-- pqueue: 233.8 ms
pqueue cs = do
  let pq = PQ.fromList cs
  let Just (v, _) = PQ.maxView pq
  print v

-- writeBinary = do
--   g  <- getStdGen
--   let rs = randomRs (0, maxSweetness) g :: [Word]
--   BS.writeFile binaryFile $ encode (take numCookies rs)

writeText :: Int -> Word -> IO ()
writeText numCookies maxSweetness = do
  g  <- getStdGen
  let rs = randomRs (0, maxSweetness) g :: [Word]
  let s =
        unlines
        [ unwords [ show numCookies, show (maxSweetness `div` 2 :: Word)]
        , unwords $ map show (take numCookies rs)
        ]
  writeFile textFile s

program = do
  which <- getArgs
  case which of
    ["heap"] -> do
      h <- openFile "text.txt" ReadMode
      run h "heap" heap

    ["binary-heap"] -> do
      h <- openFile "text.txt" ReadMode
      run h "binary-heap" binaryHeap

    ["pqueue"]-> do
      h <- openFile "text.txt" ReadMode
      run h "pqueue" pqueue

    -- ["binary"] -> writeBinary
    ["text", n] -> do
          -- maxSweetness :: Word
      let maxSweetness = 1000000000 -- 1 billion
      writeText (read n) maxSweetness

    _ -> error "no match failed"

