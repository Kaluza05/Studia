module IdMonad where

import Control.Monad

newtype Id a = Id { unId :: a } deriving Show

instance Functor Id where
  fmap f m = m >>= (return . f)

instance Applicative Id where
  pure = Id
  (<*>) = ap

instance Monad Id where
  Id x >>= f = f x