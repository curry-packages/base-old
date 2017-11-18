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
  , get, put, modify, sequence, sequence_, mapM
  , mapM_, runState, evalState, execState, liftM, liftM2
  ) where

import Control.Monad.Trans.Class
import Data.Functor.Identity

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT (\ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s)

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

{- instance foldable? -}
sequence :: Monad m => [StateT s m a] -> StateT s m [a]
sequence = foldr (\s newS -> s >>=
                    (\a -> newS >>= (\as -> return (a:as))))
                  (return [])

sequence_ :: Monad m => [StateT s m a] -> StateT s m ()
sequence_ = foldr (>>) (return ())

mapM :: Monad m => (a -> StateT s m b) -> [a] -> StateT s m [b]
mapM f = sequence . (map f)

mapM_ :: Monad m => (a -> StateT s m b) -> [a] -> StateT s m ()
mapM_ f = sequence_ . (map f)

type State s a = StateT s (Identity) a

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

liftM :: Monad m => (a -> b) -> StateT s m a -> StateT s m b
liftM f act = act >>= (return . f)

liftM2 :: Monad m => (a -> b -> c) -> StateT s m a
       -> StateT s m b -> StateT s m c
liftM2 f a b = a >>= (\x -> b >>= (\y -> return (f x y)))
