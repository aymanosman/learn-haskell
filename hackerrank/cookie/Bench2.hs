module Main where

import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), Handle)
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text as P
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
  => String -> IO a -> IO ()
time s act' =
  let act = act' >>= print
  in
  do (m, _) <- measure (nfIO act) 1
     putStr (s++": ")
     putStrLn $ secs $ measTime m

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) . decodeUtf8
  <$> B.hGetLine h

run :: Handle -> String -> IO ()
run h s =
  do
     Right [_, m] <- readWords h
     Right cs <- readWords h
     print cs
     time s (return $ ans m cs)

program = do
  which <- getArgs
  case which of
    ["binary-heap"] -> do
      h <- openFile "text.txt" ReadMode
      run h "binary-heap"

    -- ["pqueue"]-> do
    --   h <- openFile "text.txt" ReadMode
    --   run h "pqueue" pqueue

    _ -> error "no match failed"

