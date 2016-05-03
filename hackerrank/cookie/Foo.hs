-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Data.Word
import Data.Binary

import Control.Monad
import Control.Monad.ST
-- import Data.Array.ST
import Data.Array.IO
import Data.Attoparsec.Text as P
import Control.Applicative
import Data.Text.Encoding
import System.IO
import qualified Data.ByteString as B

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace)) . decodeUtf8 <$> B.hGetLine h


ff :: Word -> Word -> [Word] -> IO (IOUArray Word Word)
ff n m cs =
  do arr <- newArray (1,n) 0
     a <- readArray arr 1
     forM_ (zip [1..n] cs) (\(i, c) ->
       writeArray arr i c)
     b <- readArray arr 1
     c <- readArray arr 1000
     return arr

heapsort = undefined

main =
  -- print $ runST ff
  do h <- openFile "text.txt" ReadMode
     run h

run h = do
  Right [n, m] <- readWords h
  Right cs <- readWords h
  arr <- ff n m cs
  el <- readArray arr 1000
  print (n, m)
  print el
