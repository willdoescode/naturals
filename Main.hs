module Main where

import Prelude hiding (even, odd)

data Nat = Z | S Nat
  deriving (Eq)

show' :: Nat -> Int
show' (S rest@(.)) = 1 + show' rest
show' Z = 0

instance Show Nat where
  show = show . show'

instance Ord Nat where
  (S k) > Z = True
  (S k) > (S y) = k > y
  Z > (S k) = False

  (S k) >= Z = True
  (S k) >= (S y) = k >= y
  Z >= Z = True
  Z >= (S k) = False

  (S k) <= Z = False
  (S k) <= (S y) = k <= y
  Z <= (S k) = True
  Z <= Z = True

instance Num Nat where
  Z + n = n
  (S k) + n = S (k + n)
  
  Z * n = Z
  (S k) * n = n + (k * n)

  n - Z = n
  (S k) - (S y) = k - y
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

