module Main where

import Prelude hiding (even, odd)

data Nat = Z | S Nat

instance Eq Nat where
  (S k) == (S y) = k == y
  Z == Z = True
  Z == _ = False
  _ == Z = False

show' :: Nat -> Int
show' (S k) = 1 + show' k
show' Z = 0

instance Show Nat where
  show = show . show'

instance Ord Nat where
  (S k) > (S y) = k > y
  (S k) > Z = True
  Z > (S k) = False

  (S _) >= Z = True
  (S k) >= (S y) = k >= y
  Z >= (S _) = False
  Z >= Z = True

  (S _) <= Z = False
  (S k) <= (S y) = k <= y
  Z <= (S _) = True
  Z <= Z = True

instance Num Nat where
  Z + n = n
  (S k) + n = S (k + n)
  
  Z * n = Z
  (S k) * n = n + (k * n)

  (S k) - (S y) = k - y
  n - Z = n
  Z - n = error "Cannot have negative Nat"

  abs n = n
  signum _ = Z
  fromInteger x
    | x < 0 = error "Nats cannot be less than zero."
    | x == 0 = Z          
    | otherwise = S $ fromInteger (x - 1)

even :: Nat -> Bool
even Z = True
even (S k) = odd k

odd :: Nat -> Bool
odd Z = False
odd (S k) = even k

main :: IO ()
main = print $ (4 :: Nat) + (5 :: Nat) * (6 :: Nat)

