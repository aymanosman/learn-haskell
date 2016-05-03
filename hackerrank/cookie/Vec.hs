-- import Control.Monad
import Control.Monad.ST
import Data.Attoparsec.Text as P
-- import Control.Applicative
import Data.Text.Encoding
import System.IO
import qualified Data.ByteString as B

import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Algorithms.Heap as VH
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM

main =
  run

run = openFile "text.txt" ReadMode >>= run'

run' h = do
  Right [n, m] <- readWords h
  Right cs <- readWords h
  let v = V.fromList cs
  print $ V.take 10 $ v

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) . decodeUtf8
  <$> B.hGetLine h

-- heapsort = undefined

-- main =
--   -- print $ runST ff
--   do h <- openFile "text.txt" ReadMode
--      run h

