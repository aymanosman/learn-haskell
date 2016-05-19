module Main where

import System.IO (Handle)
import BenchBase (time, program, readWords)
import qualified Cookie2

main = do
  putStrLn "=== Cookie2 ==="
  program $ run Cookie2.ans

type Calc = Word -> [Word] -> (Word, [Word])

run :: Calc -> Handle -> IO ()
run ans h =
  do
     Right [_, m] <- readWords h
     Right cs <- readWords h
     -- print cs
     time "" (return $ ans m cs)

