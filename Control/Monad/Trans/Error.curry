--- ---------------------------------------------------------------------------
--- This monad transformer adds the ability to fail
--- or throw exceptions to a monad.
---
--- @author Bjoern Peemoeller
--- @version September 2014
--- @category general
--- ----------------------------------------------------------------------------

module Control.Monad.Trans.Error (
  module Control.Monad.Trans.Error,
  module Control.Monad.Trans.Class
  ) where

import Data.Either
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Applicative

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

-- defined in terms of the monad instance for ErrorT
instance (Functor m, Monad m, Error e) => Applicative (ErrorT e m) where
  pure = return
  f <*> v = f >>= (\f' -> v >>= (\x -> return (f' x)))

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

runError :: ErrorT e Identity a -> Either e a
runError = runIdentity . runErrorT

