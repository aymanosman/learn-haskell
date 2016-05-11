-- import Control.Applicative
import System.IO (openFile, IOMode(ReadMode), Handle)
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text as P

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Algorithms.Heap as VH

main =
  run1

run = openFile "text.txt" ReadMode >>= run'

run' h = do
  Right [n, m] <- readWords h
  Right cs <- readWords h
  let v = V.fromList cs
  print $ V.take 10 $ v

cmp :: Ord a
  => a -> a -> Ordering
cmp a b =
  case compare a b of
    EQ -> EQ
    LT -> GT
    GT -> LT

-- run1 :: IO (MV.MVector s a)
run1 = do
  let size = length xs
      xs = [1,9,10,2,12,3,33,32,89,47,75,90,44]
      p v = forM_ [0..size-1] $ \i -> print =<< (MV.unsafeRead v i)
  v <- MV.new size
  forM_ (Prelude.zip [0..size-1] xs) $ \(i, e) ->
    MV.unsafeWrite v i e

  p v
  -- VH.sortBy cmp v
  VH.sortHeap cmp v 0 1 size
  -- VH.heapify compare v 0 size
  putStrLn "==="
  -- VH.pop compare v 0 size
  -- VH.heapify compare v 0 (size-1)

  p v
  return v

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) . decodeUtf8
  <$> B.hGetLine h

