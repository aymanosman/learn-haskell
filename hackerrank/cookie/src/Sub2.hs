-- {-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment (getArgs)
import qualified Data.List as List
import Parse

main = do
  [filename] <- getArgs
  (sweetness, cookies) <- parseFile filename
  let (steps, _) = solution sweetness cookies
  print steps

solution sweetness l =
  go 0 (List.sort l)
  where
    go steps l@(x:y:xs) =
      if all (>sweetness) [x,y]
      then (steps,l)
      else go (steps+1) $ List.insert (x+2*y) xs


