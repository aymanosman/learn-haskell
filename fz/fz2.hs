import Data.Maybe
import Data.Monoid
import Control.Monad


main = run 15

run a =
 forM_ lines putStrLn
 where
   nums = [1..a]
   xs = zip nums (map fz nums)
   lines =
     map (\(a, b) -> show a <> " " <> b)
     xs


fz n =
  fromMaybe (show n) (fizzer n)


fizzer =
  mconcat
  [ (3 ~> "Fizz")
  , (5 ~> "Buzz")
  , (7 ~> "Hiss")
  ]


(~>) :: Int -> String -> Int -> Maybe String
(~>) m s n =
  if mod n m == 0
  then Just s
  else Nothing
