import Control.Monad
import Data.List
import Test.QuickCheck

main =
  -- print $ wrap 10 sss
  putStrLn $ wrap 10 sss

sss = "Four score and seven years ago our fathers brought forth upon this continent a new nation conceived in liberty and dedicated to the proposition that all men are created equal"
ttt = "Four score\nand seven\nyears ago\nour\nfathers\nbrought\nforth upon\nthis\ncontinent\na new\nnation\nconceived\nin liberty\nand\ndedicated\nto the\npropositio\nn that all\nmen are\ncreated\nequal"

test =
  [ wrap 10 sss == ttt
  , let t = lines $ wrap 10 sss in
    all (<11) $ map length t
  ]

wrap n s =
  intercalate "\n"
  $ map unwords
  $ makeUpTo n
  $ breakUpLongWords n
  $ words s

breakUpLongWords n =
  concatMap (breakUp n)

breakUp n [] = []
breakUp n s =
  if length s > n
    then let (pref,suff) = splitAt n s in pref:breakUp n suff
    else [s]

makeUpTo n xs =
  go ([], [], xs)
  where
    go (zs, ys, []) = reverse (ys:zs)
    go (zs, ys, x:xs) =
      let
        yl = length $ unwords ys
        xl = length x
      in
      case compare (yl + xl) n of
        GT ->
          go (ys:zs, [x], xs)

        EQ ->
          go ((ys ++ [x]):zs, [], xs)

        LT ->
          go (zs, ys ++ [x], xs)

check =
  let
    genPhrase = listOf $ listOf (choose ('a', 'z'))
  in
  do xs <- sample' genPhrase
     forM_ xs (putStrLn . unwords)
