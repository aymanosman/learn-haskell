import Control.Concurrent (threadDelay)
import Control.Monad (forM_, replicateM)
import Data.List
import Debug.Trace (trace)
import System.Random (randomRIO)
import System.Environment (getArgs)
import System.Console.ANSI as A
import System.IO (stdout, hSetBuffering, BufferMode(..))

main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["rand"] ->
      program
    ["main", width, max] -> do
      cs <- run (read max) (read width)
      forM_ cs pp


program :: IO ()
program = do
  cs <- run 2 10
  forM_ (take 5 cs) pp

-- run :: Int -> IO [[Int]]
run m n = do
  cells <- replicateM n (randomRIO (0,m)) :: IO [Int]
  return $ iterate (f m) cells


pp :: [Int] -> IO ()
pp cs = do
  forM_ cs (\c -> do
    setSGR [SetColor Foreground Vivid (w c)]
    let c':[] = show c
    putChar c'
    )
  -- putStrLn $ intercalate "" $ map show cs
  putStrLn ""
  threadDelay 10000
  where
    w n =
      case n of
        0 -> Black
        1 -> Red
        2 -> Green
        3 -> Yellow
        4 -> Blue
        5 -> Magenta
        6 -> Cyan
        7 -> White
        8 -> Red
        9 -> Green


f m cells =
  -- trace (show bs)
  map (g m) bs
  where
    -- bs :: [(Int,Int,Int)]
    bs =
      zip3 (last cells:cells) cells (tail cells ++ cells)

g m (l,x,r) =
  g' m (l+x+r)

ggg' m n = mod n m

g' m n =
  case n of
    0 -> 0
    1 -> 2
    2 -> 3
    3 -> 0
    4 -> 0
    5 -> 2
    6 -> 1
    7 -> 1
    8 -> 3
    9 -> 3
    _ -> 0

hg' m n =
  case n of
    0 -> 0
    1 -> 0
    2 -> 1
    3 -> 2
    4 -> 0
    5 -> 0
    6 -> 1
    _ -> 0


gg' _m n =
  case n of
    0 -> 1
    1 -> 0
    2 -> 1
    3 -> 0

-- hhg' m n =
--   case n of
--     0 -> 0
--     1 -> 2
--     2 -> 3
--     3 -> 0
--     4 -> 0
--     5 -> 1
--     6 -> 1
--     7 -> 1
--     8 -> 3
--     9 -> 3
--     _ -> 0

