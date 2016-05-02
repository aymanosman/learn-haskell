module Queue where

class Queue a where
  empty :: Ord a => Queue a
  isEmpty :: Queue a -> Bool

  snoc :: a -> Queue a -> Queue a
