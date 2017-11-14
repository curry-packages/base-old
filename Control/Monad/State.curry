------------------------------------------------------------------------------
--- This library provides an implementation of the state monad.
---
--- @author Jan-Hendrik Matthes, Bjoern Peemoeller, Fabian Skrlac
--- @version August 2016
--- @category general
------------------------------------------------------------------------------

module Control.Monad.State
  ( State
  , get, put, modify, sequence, sequence_, mapM
  , mapM_, runState, evalState, execState, liftM, liftM2
  ) where

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s a) where
  fmap f m = State (\ s -> fmap (\ ~(a, s') -> (f a, s')) (runState m s))

instance Monad State where
  m >>= k  = State (\ s -> do ~(a, s') <- runState m s
                              runState (k a) s')
  return a = State (\ s -> (a, s))

state :: (s -> (a, s)) -> State s a
state f = State (return . f)

get :: State s s
get = state (\ s -> (s, s))

put :: s -> State s ()
put s = state (\ _ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = state (\ s -> ((), f s))

{- instance foldable? -}
sequence :: [State s a] -> State s [a]
sequence = foldr (\s newS -> s >>=
                    (\a -> newS >>= (\as -> return (a:as))))
                  (return [])

sequence_ :: [State s a] -> State s ()
sequence_ = foldr (>>) (return ())

mapM :: (a -> State s b) -> [a] -> State s [b]
mapM f = sequence . (map f)

mapM_ :: (a -> State s b) -> [a] -> State s ()
mapM_ f = sequence_ . (map f)

runState :: State s a -> s -> (a, s)
runState (State m) s = m s

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

liftM :: (a -> b) -> State s a -> State s b
liftM f act = act >>= (return . f)

liftM2 :: (a -> b -> c) -> State s a -> State s b -> State s c
liftM2 f a b = a >>= (\x -> b >>= (\y -> return (f x y)))
