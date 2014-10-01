--- ----------------------------------------------------------------------------
--- Library with some useful functions on the `Maybe` datatype.
---
--- @author Frank Huch, Bernd Brassel, Björn Peemöller
--- @version October 2014
--- ----------------------------------------------------------------------------

module Maybe
  ( Maybe (..)
  , maybe
  , isJust, isNothing
  , fromJust, fromMaybe
  , listToMaybe, maybeToList
  , catMaybes, mapMaybe
  , (>>-), sequenceMaybe, mapMMaybe, mplus
  ) where

infixl 1 >>-

--- Return `True` iff the argument is of the form `Just _`.
isJust :: Maybe _ -> Bool
isJust (Just _) = True
isJust Nothing  = False

--- Return `True` iff the argument is of the form `Nothing`.
isNothing :: Maybe _ -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

--- Extract the argument from the `Just` constructor and throw an error
--- if the argument is `Nothing`.
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "Maybe.fromJust: Nothing"

--- Extract the argument from the `Just` constructor or return the provided
--- default value if the argument is `Nothing`.
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just a) = a

--- Return `Nothing` on an empty list or `Just x` where `x` is the first
--- list element.
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (a : _) = Just a

--- Return an empty list for `Nothing` or a singleton list for `Just x`.
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

--- Return the list of all `Just` values.
catMaybes :: [Maybe a] -> [a]
catMaybes ms = [ m | (Just m) <- ms ]

--- Apply a function which may throw out elements using the `Nothing`
--- constructor to a list of elements.
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f

--- Monadic bind for Maybe.
--- Maybe can be interpreted as a monad where Nothing is interpreted
--- as the error case by this monadic binding.
--- @param maybeValue - Nothing or Just x
--- @param f - function to be applied to x
--- @return Nothing if maybeValue is Nothing, otherwise f is applied to x
(>>-) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>- _ = Nothing
Just x  >>- f = f x

--- Monadic `sequence` for `Maybe`.
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
sequenceMaybe (c:cs) = c >>- \x -> sequenceMaybe cs >>- \xs -> Just (x:xs)

--- Monadic `map` for `Maybe`.
mapMMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMMaybe f = sequenceMaybe . map f

--- Combine two `Maybe`s, returning the first `Just` value, if any.
mplus :: Maybe a -> Maybe a -> Maybe a
Nothing    `mplus` y = y
x@(Just _) `mplus` _ = x
