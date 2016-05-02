module Main where

import System.Random
import System.Environment
import qualified Data.Judy as J
import Control.Monad
import qualified Data.Heap as H

import Data.Binary
import qualified Data.ByteString.Lazy as BS

-- TODO: compare to heap and stuff
n = 1000000

main = do
  [which] <- getArgs
  case which of
    "judy" -> judy
    "heap" -> heap
    _ -> error "failed"

judy = do
   g  <- getStdGen
   let rs = randoms g
   j  <- J.new :: IO (J.JudyL Int)
   forM_ (take n rs) $ \n ->
       J.insert n 1 j
   Just (v, _)  <- J.findMax j
   print v

heap = do
   g  <- getStdGen
   let rs = randoms g
   let h = H.fromList (take n rs) :: H.MaxHeap Int
   let Just w = H.viewHead h
   print w

write_nums = do
   g  <- getStdGen
   let rs = randoms g :: [Int]
   BS.writeFile "sample.txt" $ encode (take n rs)



