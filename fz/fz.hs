import Data.Maybe
import Test.QuickCheck

fz :: Int -> String
foo :: Int


fz n = fromMaybe (show n) (test n)

test n =
  if mod n 3 == 0
  then Just "Fizz"
  else Nothing


foo = 42


prop :: Int -> Bool
prop n =
  if mod n 3 == 0
  then fz n /= show n
  else fz n == show n

spec =
  quickCheck prop
