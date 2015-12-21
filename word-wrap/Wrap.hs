import Data.List

main =
  -- print $ wrap 10 sss
  putStrLn $ wrap 10 sss

sss = "Four score and seven years ago our fathers brought forth upon this continent a new nation conceived in liberty and dedicated to the proposition that all men are created equal"

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
          go ((ys ++ [take (n-yl) x]):zs, [], drop (n-yl) x:xs)

        EQ ->
          go ((ys ++ [x]):zs, [], xs)

        LT ->
          go (zs, ys ++ [x], xs)

