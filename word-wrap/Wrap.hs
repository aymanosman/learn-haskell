import Data.List

main =
  -- print $ wrap 10 sss
  putStrLn $ wrap 10 sss

sss = "Four score and seven years ago our fathers brought forth upon this continent a new nation conceived in liberty and dedicated to the proposition that all men are created equal"
ttt = "Four score\nand seven\nyears ago\nour\nfathers\nbrought\nforth upon\nthis\ncontinent\na new\nnation\nconceived\nin liberty\nand\ndedicated\nto the\npropositio\nn that all\nmen are\ncreated\nequal"

test =
  wrap 10 sss == ttt

wrap n s =
  intercalate "\n"
  $ map unwords
  $ makeUpTo n
  $ words s

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
          let
            (pref, suff) = splitAt (n-yl) x
          in
          go ((ys ++ [pref]):zs, [], suff:xs)

        EQ ->
          go ((ys ++ [x]):zs, [], xs)

        LT ->
          go (zs, ys ++ [x], xs)

