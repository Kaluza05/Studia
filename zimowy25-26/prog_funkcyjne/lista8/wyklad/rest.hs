module PF where

import Control.Monad
 
sort :: Ord a => [a] -> [a]
sort []     = []
sort [x]    = [x]
sort (x:xs) = sort [y | y <- xs, y < x] ++ [x] ++ sort [y | y <- xs, y >= x]

min :: Ord a => [a] -> a
min = head . sort

primes :: [Integer]
primes = 2:filter isPrime [3..] where
  isPrime :: Integer -> Bool
  isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p*p <= n) primes

type IntegerList = [Integer]

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)

flatTree :: Tree a -> [a]
flatTree Leaf = []
flatTree (Node l x r) = flatTree l ++ [x] ++ flatTree r

newtype IntList = IntList { unIntList :: [Int] }

data Vec2D = Vec2D { x :: Integer, y :: Integer }

newtype Id a = Id { unId :: a } deriving Show

instance Functor Id where
  fmap f m = m >>= (return . f)

instance Applicative Id where
  pure = Id
  (<*>) = ap

instance Monad Id where
  Id x >>= f = f x