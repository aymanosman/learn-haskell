module Main where

import Control.Monad (replicateM)
import System.Random (randomRIO)

n = 6
sweetness = 7

main =
  -- writeFile "cookie-input.txt" ss
  do ss <- makeString n sweetness
     putStrLn ss

makeString :: Int -> Int -> IO String
makeString n m =
  do xs <- randomInts n
     let s = unlines
             [ unwords (map show [n, m])
             , "1 2 3 9 10 11 12"
             ]
     return s

randomInts :: Int -> IO [Int]
randomInts n =
  sequence $ replicateM n (randomRIO (1,100))
