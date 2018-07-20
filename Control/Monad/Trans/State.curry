------------------------------------------------------------------------------
--- This library provides an implementation of the state monad.
---
--- @author Jan-Hendrik Matthes, Bjoern Peemoeller, Fabian Skrlac
--- @version August 2016
--- @category general
------------------------------------------------------------------------------

module Control.Monad.Trans.State
  ( State, StateT(runStateT)
  , evalStateT, execStateT
  , get, put, modify, runState, evalState, execState
  , module Control.Monad.Trans.Class
  ) where

import Control.Monad.Trans.Class
import Data.Functor.Identity
import Control.Applicative

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT (\ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s)

-- defined in terms of the monad instance for StateT
instance (Functor m, Monad m) => Applicative (StateT s m) where
  pure = return
  f <*> v = f >>= (\f' -> v >>= (\x -> return (f' x)))

instance (Monad m) => Monad (StateT s m) where
  m >>= k  = StateT (\ s -> do ~(a, s') <- runStateT m s
                               runStateT (k a) s')
  return a = StateT (\ s -> return (a, s))
  fail str = StateT (\ _ -> fail str)

instance MonadTrans (StateT s) where
  lift m = StateT (\ s -> do a <- m
                             return (a, s))

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

get :: Monad m => StateT s m s
get = state (\ s -> (s, s))

put :: Monad m => s -> StateT s m ()
put s = state (\ _ -> ((), s))

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state (\ s -> ((), f s))

type State s = StateT s Identity

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    ~(a, _) <- runStateT m s
    return a

execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    ~(_, s') <- runStateT m s
    return s'

