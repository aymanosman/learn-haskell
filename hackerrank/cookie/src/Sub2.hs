-- {-# LANGUAGE BangPatterns #-}
module Main (
  solution
) where

import qualified Data.List as List
import Parse

solution sweetness l =
  go 0 (List.sort l)
  where
    go steps l@(x:y:xs) =
      if all (>sweetness) [x,y]
      then (steps,l)
      else go (steps+1) $ List.insert (x+2*y) xs


