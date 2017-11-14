--- ---------------------------------------------------------------------------
--- A combination of Error and state monad like `ErrorT State` in Haskell.
---
--- @author Bjoern Peemoeller
--- @version September 2014
--- @category general
--- ----------------------------------------------------------------------------
-- NOTE: This module will be changed when Monad Transformers are possible

module Control.Monad.ErrorState

import Data.Either

--- Error state monad.
newtype ErrorState s a = ErrorState { runErrorState :: s -> Either String (a, s)}

class Error a where
  noMsg :: a
  noMsg = strMsg ""
  strMsg :: String -> a
  strMsg _ = noMsg

class ErrorList a where
  listMsg :: String -> [a]

instance ErrorList Char where
  listMsg = id

instance ErrorList a => Error [a] where
  strMsg = listMsg

--- Evaluate an `ES` monad
runError :: ErrorState e s a -> s -> Either e a
runError (ErrorState m) s = case m s of
  Left  e      -> Left e
  Right (x, _) -> Right x

instance Monad (ErrorState e s a) where
  return a = State (\s -> Right (a, s))
  fail str = State (\_ -> Left  (strMsg str))
  (ErrorState m) >>= k
    = State (\s -> case m s of
                    Left e -> Left e
                    Right (x, s') -> (x, s'))


--- Sequence operator of the `ES` monad
(*>) :: ErrorState e s _ -> ErrorState e s a -> ErrorState e s a
m *> n = m >>= (\_ -> n)

--- Apply a pure function onto a monadic value.
(<$>) :: (a -> b) -> ErrorState e s a -> ErrorState e s b
f <$> act = act >>= (\x -> return (f x))

--- Apply a function yielded by a monadic action to a monadic value.
(<*>) :: ErrorState e s (a -> b) -> ErrorState e s a -> ErrorState e s b
sf <*> sx = sf >>= (\f -> sx >>= (\x -> return (f x)))

--- Retrieve the current state
get :: ErrorState e s s
get = ErrorState (\s -> Right (s, s))

--- Replace the current state
puts :: s -> ErrorState e s ()
puts s = ErrorState (\_ -> Right ((), s)

--- Modify the current state
modify :: (s -> s) -> ErrorState e s ()
modify f = ErrorState (\s -> Right ((), f s))

--- Map a monadic function on all elements of a list by sequencing
--- the effects.
mapM :: (a -> ErrorState e s b) -> [a] -> ErrorState e s [b]
mapM _ [] = return []
mapM f (x:xs) = f x >>= (\x' -> (mapM f xs) >>= (\xs' -> return (x' : xs')))

--- Same as `concatMap`, but for a monadic function.
concatMapM :: (a -> ES e s [b]) -> [a] -> ES e s [b]
concatMapM f xs = concat <$> mapM f xs

--- Same as `mapES` but with an additional accumulator threaded through.
mapAccumM :: (a -> b -> ES e s (a, c)) -> a -> [b] -> ES e s (a, [c])
mapAccumM _ s []       = return (s, [])
mapAccumM f s (x : xs) = f s x >>= (\(s', x') -> (mapAccumM f s' xs) >>=
                                     (\(s'', xs') -> return (s'', x' : xs')))
