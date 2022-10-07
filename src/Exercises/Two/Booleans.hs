module Exercises.Two.Booleans
  ( isAscending4,
    swap,
    rotate,
    myCurry,
    myUncurry,
    myFlip,
    mapPair,
    mapPair2,
    f,
  )
where

-- 1.
isAscending4 :: Int -> Int -> Int -> Int -> Bool
isAscending4 x1 x2 x3 x4 = x4 > x3 && x3 > x2 && x2 > x1

-- TODO: Challenge: Try and define isAscendingN where it can take up to N arguments

-- 2.
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- 3.
rotate :: (a, b, c) -> (c, a, b)
rotate (a, b, c) = (c, a, b)

-- 4.
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry func a b = func (a, b)

-- 5.
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry func (a, b) = func a b

-- 6.
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip func b a = func a b

-- 7.
mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f1 f2 (a, b) = (f1 a, f2 b)

-- 8.
mapPair2 :: ((a, b) -> (c, d)) -> (a, b) -> (c, d)
mapPair2 func (a, b) = func (a, b)

-- 9.
f :: (a, b) -> (a -> c) -> (b -> d) -> ((c, d) -> e) -> e
f (a, b) f1 f2 f3 = f3 (f1 a, f2 b)

-- 10.
-- Tuple
-- - You know the number of values you're storing
-- - The types of the values are different

-- List
-- - You don't know the length in advance
-- - You have an ordered sequence of stuff with the same type
-- - You want to operate uniformly over the data

-- Functions for Tuples: fst, snd
-- Functions for Lists: head, tails
