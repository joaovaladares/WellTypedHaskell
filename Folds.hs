module Folds where

import Prelude hiding (elem, map, and, or, filter)

elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem y (x : xs) = y == x || elem y xs

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

and :: [Bool] -> Bool
and []       = True
and (x : xs) = x && and xs

-- same as foldr (but less general since foldr works on any Foldable)
fn :: (a -> r -> r) -> r -> [a] -> r
fn cons nil []       = nil
fn cons nil (x : xs) = cons x (fn cons nil xs)

-- another way to write foldr
fn' :: (a -> r -> r) -> r -> [a] -> r
fn' cons nil = go
  where
    go []       = nil
    go (x : xs) = cons x (go xs)

elem' :: Eq a => a -> [a] -> Bool
elem' y = fn (\ x r -> x == y || r) False

map' :: (a -> b) -> [a] -> [b]
map' f = fn (\ x r -> f x : r ) []

and' :: [Bool] -> Bool
and' = fn (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\ x r -> if f x then x : r else r) []

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\ x r -> f x || r) False

length' :: [a] -> Int
length' = foldr (\ _ r -> 1 + r) 0
