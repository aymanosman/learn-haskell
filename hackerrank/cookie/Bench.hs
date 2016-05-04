module Main where

import System.Random
import System.Environment

import Data.Binary

import System.IO (hGetLine, hPutStr, openFile, IOMode(ReadMode), Handle)
import qualified Data.ByteString.Lazy as BS
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

-- TODO: compare to heap and stuff
binaryFile, textFile :: String
binaryFile = "binary.txt"
textFile = "text.txt"
maxSweetness :: Word
maxSweetness = 500000000 -- 500 million

-- time :: IO a -> IO ()
time s act =
  do (m, d) <- measure (nfIO act) 1
     putStr (s++": ")
     putStrLn $ secs $ measTime m

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) . decodeUtf8 <$> B.hGetLine h

readWordsSlow :: Handle -> IO [Word]
readWordsSlow h =
  fmap read . words <$> hGetLine h

runSlow h s f =
  do [n, m] <- readWordsSlow h
     cs <- readWordsSlow h
     print (n, m)
     time s (f cs)

run :: Handle -> String -> ([Word] -> IO ()) -> IO ()
run h s f =
  do
     ret1 <- readWords h
     ret2 <- readWords h
     either error (\[_, _] ->
       either error (time s . f) ret2) ret1


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
    ["text", n] ->
      writeText (read n) maxSweetness

    _ -> error "no match failed"

main :: IO ()
main = program

test =
 do h <- openFile "text.txt" ReadMode
    -- slow pqueue (with read instead of attoparsec) took 4.912 s
    -- while fast took 236.6 ms, parsing with attoparsec is a lot faster
    -- TODO: test normal parsec
    runSlow h "pqueue" pqueue
