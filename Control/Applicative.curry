module Control.Applicative where

infixl 4 <*>, <*, *>

--- Typeclass for Applicative-Functors
class Functor f => Applicative f where
  pure  :: a -> f a

  (<*>) :: f (a -> b) -> f a -> f b

  (*>)  :: f a        -> f b -> f b
  m *> n = (id <$ m) <*> n

  (<*)  :: f a        -> f b -> f a
  (<*) = liftA2 const

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f x = (<*>) (fmap f x)

instance Applicative Maybe where
  pure = Just

  Just f  <*> Just a  = Just (f a)
  Nothing <*> Just _  = Nothing
  Just _  <*> Nothing = Nothing
  Nothing <*> Nothing = Nothing

--- Lift a function to actions.
--- This function may be used as a value for `fmap` in a `Functor` instance.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

--- Lift a ternary function to actions.
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c
