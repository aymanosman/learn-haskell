import Data.Maybe

main =  mapM_ (putStrLn . fz) [1..100]

fz n = fromMaybe (show n) (fizzer n)

fizzer = mconcat [3 ~> "Fizz" , 5 ~> "Buzz" , 7 ~> "Hiss"]

(~>) m s n = if mod n m == 0 then Just s else Nothing
