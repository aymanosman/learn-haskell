-- {-# LANGUAGE DeriveAnyClass #-}
module Play where

import Data.Semigroup

main =
  do
    let t :: Tree Priority String
        t = union (leaf 2 "a") (leaf 5 "b")
        u = union (leaf 1 "c") (leaf 8 "d")
    print (t <> u)
    print $ winner t

leaf n = Leaf (Priority n)
union l@(Leaf v a) r@(Leaf w b) =
  Branch (v <> w) l r
union l@(Branch n a b) r@(Branch m c d) =
  Branch (n <> m) l r

-- make v w =
--   Branch (v <> w)

-- union l@(Branch v a) r@(Branch w b) =
newtype Size = Size Int
-- type Priority = Int
newtype Priority = Priority Int deriving (Eq, Ord, Show, Bounded)
-- instance Monoid Size where
--   mempty = 0
--   mappend = (+)

instance Monoid Priority where
  mempty = maxBound
  mappend = min
instance Semigroup Priority

instance (Bounded v, Ord v, Semigroup v, Monoid a, Ord a)
  => Monoid (Tree v a) where
  mempty = Leaf maxBound mempty
  mappend = union
instance (Bounded v, Ord v, Semigroup v, Monoid a, Ord a)
  => Semigroup (Tree v a)


data Tree v a =
  Leaf v a
  | Branch v (Tree v a) (Tree v a)
  deriving (Show)

winner :: Tree Priority a -> a
winner t =
  go t
  where
    go (Leaf _ a) = a
    go (Branch _ x y)
      | tag x == tag t = go x
      | tag y == tag t = go y

tag (Leaf v _) = v
tag (Branch v _ _) = v
