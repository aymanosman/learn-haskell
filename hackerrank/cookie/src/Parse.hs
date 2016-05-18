module Parse (
  parse
  , parseFile
) where

import System.IO (openFile, IOMode(ReadMode), Handle)
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString.Char8 as P

readWords :: Handle -> IO (Either String [Word])
readWords h =
  P.parseOnly (P.many1 (P.decimal <* P.skipSpace))
  <$> B.hGetLine h

parse :: Handle -> IO (Word, [Word])
parse h = do
  Right [_, sweetness] <- readWords h
  Right cs <- readWords h
  return $ (sweetness, cs)

parseFile :: String -> IO (Word, [Word])
parseFile filename = do
  h <- openFile filename ReadMode
  parse h

