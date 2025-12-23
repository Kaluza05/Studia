module StreamMonad where
import Control.Monad


data StreamTrans i o a = 
    Return a
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)


instance Functor (StreamTrans i o) where
    fmap f (Return a)       = Return (f a)
    fmap f (ReadS k)        = ReadS (fmap f . k )
    fmap f (WriteS o rest)  = WriteS o (fmap f rest)

instance Applicative (StreamTrans i o) where
  pure = Return
  (<*>) = ap

instance Monad (StreamTrans i o) where
  return = pure
  (>>=) (Return a) f = f a
  (>>=) (ReadS s) f = ReadS(\j -> s j >>= f)
  (>>=) (WriteS o c) f = WriteS o (c >>= f)
