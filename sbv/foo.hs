{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main =
  print =<< x

x =
  sat . forSome ["x", "y"] $ \ (x::SInteger) y ->
    x^2 + y^2 .== 25
    &&&
    3 * x + 4 * y .== 0

