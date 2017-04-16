module Utility where

data Result a = Result a | Failure String

instance Functor Result where
  fmap f (Result a) = Result (f a)
  fmap _ (Failure errmsg) = Failure errmsg

instance Applicative Result where
  pure a = Result a
  (Failure errmsg) <*> _ = Failure errmsg
  (Result f) <*> a = fmap f a

instance Monad Result where
  (Result a) >>= f = f a
  (Failure errmsg) >>= _ = Failure errmsg
  return a = Result a
  fail err = Failure err

instance (Show a) => Show (Result a) where
  show (Result a) = show a
  show (Failure f) = "Error: " ++ f
