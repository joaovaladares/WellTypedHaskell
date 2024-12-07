{-# OPTIONS_GHC -Wall #-}

module Tables where

import           Prelude hiding (curry, fst, snd, uncurry, zip, zipWith)

data Pair a b  = Pair a b
    deriving Show

fst :: Pair a b -> a
fst (Pair x _) = x

fst' :: (a, b) -> a
fst' (x, _) = x

snd :: Pair a b -> b
snd (Pair _ y) = y

snd' :: (a, b) -> b
snd' (_, y) = y

fromPair :: Pair a b -> (a, b)
fromPair (Pair x y) = (x, y)

toPair :: (a, b) -> Pair a b
toPair (x, y) = Pair x y

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

swap' :: Pair a b -> Pair b a
swap' (Pair x y) = Pair y x

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip  _        _       = []

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith (,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys
zipWith _  _         _      = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (uncurry f) (zip xs ys)
