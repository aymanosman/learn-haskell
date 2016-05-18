module Main where

import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), Handle)
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text as P

data Ans =
  Done (BinaryHeap Word)
  | More (BinaryHeap Word)
  | Fail

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) . decodeUtf8 <$> B.hGetLine h

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] ->
      program file

    _ ->
      putStrLn "Usage: ..."

program :: String -> IO ()
program filename = do
  h <- openFile filename ReadMode
  Right [_, sweetness] <- readWords h
  Right cs <- readWords h
  -- assert length cs == _ above
  -- print $ take 10 cs
  case ans sweetness cs of
    Nothing ->
      putStrLn "-1"
    Just n ->
      print n

ans :: Word -> [Word] -> Maybe Word
ans m l =
  case ans' 0 m (fromList l) of
    Nothing -> Nothing
    Just (n, _) -> Just n

ans' :: Word -> Word -> BinaryHeap Word -> Maybe (Word, BinaryHeap Word)
ans' n sweetness h =
  case step sweetness h of
    Fail -> Nothing

    Done heap ->
      Just (n, heap)

    More heap ->
      ans' (n+1) sweetness heap

step :: Word -> BinaryHeap Word -> Ans
step m heap
  | _size heap < 2 =
    case viewHead heap of
      Nothing -> Fail
      Just n ->
        if n >= m
        then Done heap
        else Fail
  | otherwise =
    let Just (a, h) = view heap
        Just (b, h') = view h
    in
      if a >= m && b >= m
      then Done heap
      else More $ insert (a+2*b) h'

-- tests

-- min heap implementation

data BinaryHeap a =
  Empty
  | Node
    { _size :: !Word
    , _val :: !a
    , _left :: BinaryHeap a
    , _right :: BinaryHeap a
    }
  deriving (Show)

instance Heap BinaryHeap where
  isEmpty Empty = True
  isEmpty _ = False
  empty = Empty

  findMin = viewHead
  deleteMin Empty = error "empty"
  deleteMin h =
    merge (_left h) (_right h)

  insert n = merge (singleton n)
  merge Empty h = h
  merge h Empty = h
  merge h g
   | _val h < _val g = merge' h g
   | otherwise = merge' g h

singleton :: Ord a =>  a -> BinaryHeap a
singleton a =
  Node 1 a empty empty
{-# INLINE singleton #-}


view :: BinaryHeap Word -> Maybe (Word, BinaryHeap Word)
view (Node _size n l r) = Just (n, merge l r)
view _ = Nothing
{-# INLINE view #-}

viewHead :: Ord a => BinaryHeap a -> Maybe a
viewHead Empty = Nothing
viewHead heap = Just $ _val heap

merge' h g =
  Node
    (_size h + _size g)
    (_val h)
    (merge g (_left h))
    (_right h)
{-# INLINE merge' #-}

-- Conversions
fromList :: [Word] -> BinaryHeap Word
fromList =
  foldr (merge . singleton) Empty

toList :: BinaryHeap Word -> [Word]
toList Empty = []
toList (Node _ a l r) =
  a : toList (merge l r)


class Heap h where
  empty :: Ord a => h a
  isEmpty :: Ord a => h a -> Bool

  insert :: Ord a => a -> h a -> h a
  merge :: Ord a => h a -> h a -> h a

  findMin :: Ord a => h a -> Maybe a
  deleteMin :: Ord a => h a -> h a
