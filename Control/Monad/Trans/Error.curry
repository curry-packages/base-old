--- ---------------------------------------------------------------------------
--- This monad transformer adds the ability to fail
--- or throw exceptions to a monad.
---
--- @author Bjoern Peemoeller
--- @version September 2014
--- @category general
--- ----------------------------------------------------------------------------

module Control.Monad.Trans.Error where

import Data.Either
import Control.Monad.Trans.Class

--- Error monad.
newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

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

instance (Functor m) => Functor (ErrorT e m) where
    fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Monad m, Error e) => Monad (ErrorT e m) where
    return a = ErrorT $ return (Right a)
    m >>= k  = ErrorT (do a <- runErrorT m
                          case a of
                            Left  l -> return (Left l)
                            Right r -> runErrorT (k r))
    fail msg = ErrorT $ return (Left (strMsg msg))

instance MonadTrans (ErrorT e) where
    lift m = ErrorT (do a <- m
                        return (Right a))

-- Signal an error value e.
throwError :: (Monad m) => e -> ErrorT e m a
throwError l = ErrorT $ return (Left l)

-- Handle an error.
catchError :: (Monad m) => ErrorT e m a -> (e -> ErrorT e m a) -> ErrorT e m a
catchError m h = ErrorT (do a <- runErrorT m
                            case a of
                              Left  l -> runErrorT (h l)
                              Right r -> return (Right r))

--- Sequence operator of the ErrorT monad
(*>) :: (Error e, Monad m) => ErrorT e m a -> ErrorT e m b -> ErrorT e m b
m *> n = m >>= (\x -> n)

--- Apply a pure function onto a monadic value.
(<$>) :: (Error e, Monad m) => (a -> b) -> ErrorT e m a -> ErrorT e m b
f <$> act = act >>= (\x -> return (f x))

--- Apply a function yielded by a monadic action to a monadic value.
(<*>) :: (Error e, Monad m) => ErrorT e m (a -> b)
      -> ErrorT e m a -> ErrorT e m b
f <*> v = f >>= (\f' -> v >>= (\x -> return (f' x)))

--- Same as `concatMap`, but for a monadic function.
concatMapM :: (Error e, Monad m) => (a -> ErrorT e m [b])
           -> [a] -> ErrorT e m [b]
concatMapM f xs = concat <$> mapM f xs

--- Same as `mapM` but with an additional accumulator threaded through.
mapAccumM :: (Error e, Monad m) => (a -> b -> ErrorT e m (a, c))
          -> a -> [b] -> ErrorT e m (a, [c])
mapAccumM _ s []       = return (s, [])
mapAccumM f s (x : xs) = f s x >>= (\(s', x') -> (mapAccumM f s' xs) >>=
                                     (\(s'', xs') -> return (s'', x' : xs')))
