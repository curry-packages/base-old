module Data.Foldable where

class Foldable t where
  foldr  :: (a -> b -> b) -> b -> t a -> b
  foldl  :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a)      -> t a -> a
  foldl1 :: (a -> a -> a)      -> t a -> a
