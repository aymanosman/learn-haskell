{-# LANGUAGE BangPatterns #-}
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
    -- go steps l@(x:y:xs) = -- non strict version
    go !steps l@(x:y:xs) =
      if all (>sweetness) [x,y]
      then (steps,l)
      else go (steps+1) $ List.insert (x+2*y) xs


{-

(non-strict) bin/sub2 priv/text100_000.txt +RTS -s -p -hd
30182
 108,379,332,168 bytes allocated in the heap
 123,031,649,064 bytes copied during GC
      13,592,000 bytes maximum residency (9576 sample(s))
       3,057,856 bytes maximum slop
              38 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     147930 colls,     0 par   51.380s  51.169s     0.0003s    0.0014s
  Gen  1      9576 colls,     0 par   47.264s  47.650s     0.0050s    0.0128s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   30.200s  ( 31.933s elapsed)
  GC      time   97.580s  ( 97.746s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    1.064s  (  1.073s elapsed)
  EXIT    time    0.004s  (  0.001s elapsed)
  Total   time  128.848s  (129.680s elapsed)

  %GC     time      75.7%  (75.4% elapsed)

  Alloc rate    3,588,719,608 bytes per MUT second

  Productivity  23.4% of total user, 23.3% of total elapsed

(strict) bin/sub2 priv/text100_000.txt +RTS -s -p -hd
30182
 108,376,486,528 bytes allocated in the heap
 121,953,603,448 bytes copied during GC
      13,592,000 bytes maximum residency (10768 sample(s))
       3,057,856 bytes maximum slop
              38 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     146728 colls,     0 par   52.372s  52.732s     0.0004s    0.0020s
  Gen  1     10768 colls,     0 par   50.208s  50.440s     0.0047s    0.0129s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   32.068s  ( 33.708s elapsed)
  GC      time  101.532s  (102.145s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    1.048s  (  1.027s elapsed)
  EXIT    time    0.000s  (  0.001s elapsed)
  Total   time  134.648s  (135.855s elapsed)

  %GC     time      75.4%  (75.2% elapsed)

  Alloc rate    3,379,583,588 bytes per MUT second

  Productivity  23.8% of total user, 23.6% of total elapsed

 -}
