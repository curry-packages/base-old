--- ----------------------------------------------------------------------------
--- Library with some useful operations for the `Either` data type.
---
--- @author  Björn Peemöller
--- @version September 2014
--- ----------------------------------------------------------------------------
module Either
  ( Either (..)
  , either
  , lefts
  , rights
  , isLeft
  , isRight
  , partitionEithers
  ) where

--- Extracts from a list of `Either` all the `Left` elements in order.
lefts   :: [Either a b] -> [a]
lefts x = [a | Left a <- x]

--- Extracts from a list of `Either` all the `Right` elements in order.
rights   :: [Either a b] -> [b]
rights x = [a | Right a <- x]

--- Return `True` if the given value is a `Left`-value, `False` otherwise.
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

--- Return `True` if the given value is a `Right`-value, `False` otherwise.
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

--- Partitions a list of `Either` into two lists.
--- All the `Left` elements are extracted, in order, to the first
--- component of the output.  Similarly the `Right` elements are extracted
--- to the second component of the output.
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers = foldr (either left right) ([],[])
 where
  left  a (l, r) = (a:l, r)
  right a (l, r) = (l, a:r)
