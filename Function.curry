--- ----------------------------------------------------------------------------
--- This module provides some utility functions for function application.
---
--- @author Bjoern Peemoeller
--- @version July 2013
--- @category general
--- ----------------------------------------------------------------------------
module Function where

--- `fix f` is the least fixed point of the function `f`,
--- i.e. the least defined `x` such that `f x = x`.
fix :: (a -> a) -> a
fix f = let x = f x in x

--- `(*) \`on\` f = \\x y -> f x * f y`.
--- Typical usage: `sortBy (compare \`on\` fst)`.
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y

--- Apply a function to the first component of a tuple.
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

--- Apply a function to the second component of a tuple.
second :: (a -> b) -> (c, a) -> (c, b)
second f (x, y) = (x, f y)

--- Apply two functions to the two components of a tuple.
(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f *** g) (x, y) = (f x, g y)

--- Apply two functions to a value and returns a tuple of the results.
(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f &&& g) x = (f x, g x)

--- Apply a function to both components of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)
