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

-- run1 :: IO (MV.MVector s a)
run1 = do
  let size = 6
  v <- MV.new size
  forM_ (Prelude.zip [0..size-1] [1,9,10,2,12,3]) $ \(i, e) ->
    MV.unsafeWrite v i e
  VH.sort v
  forM_ [0..size-1] $ \i -> print =<< (MV.unsafeRead v i)
  putStrLn "==="
  -- pop compare v 0 size-1
  -- heapify compare v 0 size-1

  forM_ [0..size-1] $ \i -> print =<< (MV.unsafeRead v i)
  return v

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) . decodeUtf8
  <$> B.hGetLine h

