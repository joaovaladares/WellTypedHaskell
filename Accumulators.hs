{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Accumulators where

import           Prelude hiding (length)

slowReverse :: [a] -> [a]
slowReverse []       = []
slowReverse (x : xs) = slowReverse xs ++ [x]

-- slowReverse [1..10000000]
-- = slowReverse (1 : [2..10000000])
-- = slowReverse ([2..10000000] ++ [1])

fastReverse :: [a] -> [a] -> [a]
fastReverse acc []       = acc 
fastReverse acc (x : xs) = fastReverse (x : acc) xs

reverse :: [a] -> [a]
reverse = fastReverse []

data Nat = Zero | Succ Nat
  deriving (Show)

add1 :: Nat -> Nat -> Nat
add1 Zero     n = n
add1 (Succ n) m = Succ (add1 n m)

add2 :: Nat -> Nat -> Nat
add2 Zero     n = n
add2 (Succ n) m = add2 n (Succ m)

sum1 :: Num a => [a] -> a
sum1 []       = 0
sum1 (x : xs) = x + sum1 xs

--   sum1 [1..3]
-- = sum1 (enumFromTo 1 3)
-- = sum1 (1 : enumFromTo 2 3)
-- = 1 + sum1 (enumFromTo 2 3)
-- = 1 + sum1 (2 : enumFromTo 3 3)
-- = 1 + (2 + sum1 (enumFromTo 3 3))
-- = 1 + (2 + sum1 (3 : enumFromTo 4 3))
-- = 1 + (2 + (3 + sum1 (enumFromTo 4 3)))
-- = 1 + (2 + (3 + sum1 []))
-- = 1 + (2 + (3 + 0))
-- = 1 + (2 + 3)
-- = 1 + 5
-- = 6

sumAux2 :: Num a => a -> [a] -> a
sumAux2 acc []       = acc
sumAux2 acc (x : xs) = sumAux2 (acc + x) xs

sum2 :: Num a => [a] -> a
sum2 = sumAux2 0

--   sum2 [1..3]
-- = sum2 (enumFromTo 1 3)
-- = sumAux2 0 (enumFromTo 1 3)
-- = sumAux2 0 (1 : enumFromTo 2 3)
-- = sumAux2 (0 + 1) (enumFromTo 2 3)
-- = sumAux2 (0 + 1) (2 : enumFromTo 3 3)
-- = sumAux2 ((0 + 1) + 2) (enumFromTo 3 3)
-- = sumAux2 ((0 + 1) + 2) (3 : enumFromTo 4 3)
-- = sumAux2 (((0 + 1) + 2) + 3) (enumFromTo 4 3)
-- = sumAux2 (((0 + 1) + 2) + 3) []
-- = (((0 + 1) + 2) + 3) + 0
-- = ((1 + 2) + 3) + 0
-- = (3 + 3) + 0
-- = 6 + 0
-- = 6

-- Using bang patterns
sumAux3 :: Num a => a -> [a] -> a
sumAux3 !acc []       = acc
sumAux3 !acc (x : xs) = sumAux3 (acc + x) xs

sum3 :: Num a => [a] -> a
sum3 = sumAux3 0

--   sum3 [1..3]
-- = sum3 (enumFromTo 1 3)
-- = sumAux3 0 (enumFromTo 1 3)
-- = sumAux3 0 (1 : enumFromTo 2 3)
-- = sumAux3 (0 + 1) (enumFromTo 2 3)
-- = sumAux3 1 (2 : enumFromTo 3 3)
-- = sumAux3 (1 + 2) (enumFromTo 3 3)
-- = sumAux3 3 (3 : enumFromTo 4 3)
-- = sumAux3 (3 + 3) (enumFromTo 4 3)
-- = sumAux3 6 []
-- = 6

lengthAux :: Int -> [a] -> Int
lengthAux acc []       = acc
lengthAux acc (_ : xs) = lengthAux (acc + 1) xs

-- Overflows with 1,000,000 elements if ghci is run with -K1M
length :: [a] -> Int
length = lengthAux 0 

lengthAux' :: Int -> [a] -> Int
lengthAux' !acc []       = acc
lengthAux' !acc (_ : xs) = lengthAux' (acc + 1) xs

-- Does not overflow with 1,000,000 elements if ghci is run with -K1M
-- So use bang patterns as a general rule for accumulators (making them strict)
length' :: [a] -> Int
length' = lengthAux' 0
