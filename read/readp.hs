import Text.ParserCombinators.ReadP

main =
  do putStr "input: "
     s <- getLine
     case readP_to_S parse' s of
       [((n, m), "")] ->
         print (n, m)
       _ ->
         putStrLn "invalid input"

readInt = readS_to_P reads :: ReadP Int

parse' :: ReadP (Int, Int)
parse' =
  do n <- readInt
     m <- readInt
     skipSpaces
     return (n, m)
