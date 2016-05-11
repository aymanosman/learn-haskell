module Main where

import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), Handle)
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Monad (void)

import Cookie2

import Criterion.Main (nfIO)
import Criterion.Measurement (measure, secs)
import Criterion.Types (Measured(..))
import Control.DeepSeq (NFData)

textFile :: String
textFile = "text.txt"

main, program :: IO ()
main = program

time :: (NFData a, Show a)
  => String -> IO (a, b) -> IO ()
time s act' =
  let act = act' >>= (\(n, l) -> print n)
  in
  do (m, _) <- measure (nfIO act) 1
     putStr (s++": ")
     putStrLn $ secs $ measTime m

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) <$> B.hGetLine h

run :: Handle -> String -> IO ()
run h s =
  do
     Right [_, m] <- readWords h
     Right cs <- readWords h
     -- print cs
     time s (return $ ans m cs)

program = do
  which <- getArgs
  case which of
    ["binary-heap", textFile] -> do
      h <- openFile textFile ReadMode
      run h "binary-heap"

    ["binary-heap"] -> do
      h <- openFile "text.txt" ReadMode
      run h "binary-heap"

    ["all"] -> do
      h <- openFile "text100.txt" ReadMode
      i <- openFile "text1_000.txt" ReadMode
      -- j <- openFile "text10_000.txt" ReadMode
      putStrLn "With 100"
      run h "binary-heap"
      putStrLn "With 1000"
      run i "binary-heap"
      -- putStrLn "With 10,000"
      -- run j "binary-heap"

    -- ["pqueue"]-> do
    --   h <- openFile "text.txt" ReadMode
    --   run h "pqueue" pqueue

    _ -> error "no match failed"

