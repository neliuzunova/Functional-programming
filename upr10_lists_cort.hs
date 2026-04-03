--module Basics2 where

--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- cover all cases!
--{-# OPTIONS_GHC -fwarn-unused-matches #-} -- use all your pattern matches!
--{-# OPTIONS_GHC -fwarn-missing-signatures #-} -- write all your toplevel signatures!
--{-# OPTIONS_GHC -fwarn-name-shadowing #-} -- use different names!
--{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- no incomplete patterns in lambdas!


--import Prelude hiding (sum, length, maximum, elem, reverse, drop, concat, zipWith,  gcd, lcm, length, maximum, map, filter, foldr, id)

dist' :: Double -> Double -> Double -> Double -> Double
dist' x1 y1 x2 y2 = sqrt (square dx + square dy)
  where dx = x1 - x2
        dy = y1 - y2
        square x = x * x

id :: a -> a
id x = x

gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

lcm :: Int -> Int -> Int
lcm a b = (a * b) `div` gcd a b

repeated :: Int -> (a -> a) -> a -> a
repeated 0 _ = id
repeated n f = f . (repeated (n-1) f)


first :: (a,b,c) -> a
first (x,_,_) = x
second :: (a,b,c) -> b
second (_,y,_) = y
third :: (a,b,c) -> c
third (_,_,z) = z

-- [1,2,3,4] !! 2 -> 3

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x:filter p xs
                         else filter p xs
{-
sum :: [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs


isSorted [x] = True
isSorted (x:y) = x < y
isSorted (x:y:xs) = x < y && isSorted(y:xs)
-}

quickSort [] = [] 
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort more 
  where less = filter (<=x) xs
        more = filter (>x ) xs

reverse a 
  | null a = a 
  | otherwise = reverse (tail a) ++ [head a]

--head, tail, null, length, reverse , ++ , !! 
--[ 2 * x | x <- [1..5] ]

--ones :: [a]
--ones = 1 : ones

repeat1 = 1:repeat1
