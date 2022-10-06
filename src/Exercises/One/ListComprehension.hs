module Exercises.One.ListComprehension
  ( divisibleByThree,
    triangles,
    isPrime,
    primes,
    flatten,
  )
where

-- 1.
divisibleByThree :: [Int]
divisibleByThree = [x | x <- [1 .. 30], x `mod` 3 == 0]

-- 2.
triangles :: Int -> [Int]
triangles n = [y | x <- [1 .. n], y <- [x * (x + 1) `div` 2]]

-- 3.
isPrime :: Int -> Bool
isPrime x = [y | y <- [2 .. x - 1], x `mod` 1 == 0 && x `mod` y == 0] == []

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], isPrime x]

-- 4.
flatten :: [[a]] -> [a]
flatten nested = [i | j <- nested, i <- j]
