module Main where

import Data.Maybe
import Data.List -- (sort)
import Control.Monad
import System.Random as R
-- import qualified Data.Heap as H
import Data.Heap

(&) = flip ($)

main :: IO ()
main =
  do l <- randomList
     let q = new l
         h = fromList l :: MinHeap Int
     print $ undo q
     print $ toList h
     let (e, q1) = fromMaybe (error "empty") $ get q
     print e
     print q1

data PQ a =
  PQ [a] deriving (Show)

new l =
 PQ $ sort l

get (PQ l) =
  case l of
    [] ->
      Nothing
    (x:xs) ->
      Just (x, PQ xs)

put x (PQ l) =
  PQ $ sort $ x:l

undo (PQ l) =
  l

randomQueue :: IO (PQ Int)
randomQueue =
  new <$> randomList

randomList =
  replicateM 10 (randomRIO (1,100))
