module Main where

import System.Environment (getArgs)
import System.Random
import Control.Monad

import qualified Data.List as List
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.MArray.Heapsort (sort)

import qualified Data.PQueue.Max as PQ

import Criterion.Main
import Criterion.Measurement
import Criterion.Types (Measured(..))
import Control.DeepSeq (NFData)

main :: IO ()
main =
  program

program = do
  [n'] <- getArgs
  run (read n')

run n' = do
  let n = n'*10
  putStrLn $ "n: " ++ show n
  time "list" (runList n)
  time "array" (runArray n)

time :: (NFData a) => String -> IO a -> IO ()
time s act =
  do (m, _) <- measure (nfIO act) 1
     putStr (s++": ")
     putStrLn $ secs $ measTime m

runList size = do
  xs <- replicateM size $ randomRIO (1,100) :: IO [Int]
  print $ (List.sort xs) !! (size`div`2)

runArray size = do
  xs <- replicateM size $ randomRIO (1,100) :: IO [Int]
  let arr2 = gg size xs
  print $ arr2 ! (size`div`2)

gg :: Int -> [Int] -> UArray Int Int
gg size xs = runSTUArray $ do
  arr <- newListArray (1,size) xs
  sort arr
  return arr


