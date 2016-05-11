module BenchBase where

import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), Handle)
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString.Char8 as P

import Criterion.Main (nfIO)
import Criterion.Measurement (measure, secs)
import Criterion.Types (Measured(..))
import Control.DeepSeq (NFData)

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

program run = do
  which <- getArgs
  case which of

    ["all"] -> do
      h <- openFile "text100.txt" ReadMode
      i <- openFile "text1_000.txt" ReadMode
      j <- openFile "text10_000.txt" ReadMode
      putStrLn "With 100"
      run h
      putStrLn "With 1000"
      run i
      putStrLn "With 10,000"
      run j

    _ -> error "no match failed"
