data Promise a
  = Done a
  | Failed
  deriving (Show)

f :: a -> Promise a
f = Done

-- g -> inc, h -> incP

g :: Int -> Int
g = (+1)

h :: Int -> Promise Int
h a = Done (a+1)

andThen' :: (a -> b) -> Promise a -> Promise b
andThen' f p =
  case p of
    Failed ->
      Failed

    Done a ->
      Done (f a)
-- Done 2 `andThen'` g `andThen'` g -- try h

map' = andThen'

-- andThen :: Promise a -> (a -> Promise b) -> Promise b
andThen p f =
  case p of
    Failed ->
      Failed

    Done a ->
      f a
-- Done 2 `andThen` h `andThen` h

(>>>=) = andThen
-- Done 2 >>>= h >>>= h


instance Functor Promise where
  fmap = andThen'

instance Applicative Promise where
  pure = Done
  x <*> y =
    case x of
      Failed ->
        Failed

      Done f ->
        fmap f y


instance Monad Promise where
  return = Done
  m >>= f = andThen m f


main = undefined


