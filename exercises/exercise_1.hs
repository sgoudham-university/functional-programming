import Test.HUnit

-- FUNCTIONS --
-- 1.
max2 :: Int -> Int -> Int
max2 x y = if x >= y then x else y

-- 2.
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

-- 3.
f :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
f f1 f2 = \x -> f2 (f1 x)

-- 4.
g :: (Int -> Bool) -> (Bool -> String) -> Int -> String
g f1 f2 int = f2 (f1 int)

-- 5.
twice :: (Int -> Int) -> Int -> Int
twice f1 int = f1 (f1 int)

-- 6.
forceLet :: Fractional a => a -> a -> a -> a
forceLet m1 m2 d =
  let g = 6.67 * (10 ^^ (-11))
      top = m1 * m2
      bottom = d * d
   in g * (top / bottom)

forceWhere :: Fractional a => a -> a -> a -> a
forceWhere m1 m2 d = g * ((m1 * m2) / (d * d))
  where
    g = 6.67 * (10 ^^ (-11))

-- LIST COMPREHENSION --
-- 1.
divisibleByThree :: [Int]
divisibleByThree = [x | x <- [1 .. 30], x `mod` 3 == 0]

-- 2.
triangles :: Int -> [Int]
triangles n = [x | x <- [1 .. n], x <- [x * (x + 1) `div` 2]]

-- 3.
isPrime :: Int -> Bool
isPrime x = [y | y <- [2 .. x - 1], x `mod` 1 == 0 && x `mod` y == 0] == []

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], isPrime x]

-- 4.
flatten :: [[a]] -> [a]
flatten nested = [i | j <- nested, i <- j]

-- HELPERS --
stringToBool s = length s == 5

intToBool i = i == 10

timesByTwo x = x * 2

-- TESTS --
max2Happy = TestCase (assertEqual "max2 10 100" 100 (max2 10 100))

max3Happy = TestCase (assertEqual "max3 10 100 1000" 1000 (max3 10 100 1000))

fHappy = TestCase (assertEqual "f show stringToBool 10000" True (f show stringToBool 10000))

gHappy = TestCase (assertEqual "g intToBool show 10" "True" (g intToBool show 10))

twiceHappy = TestCase (assertEqual "twice timesByTwo 5" 20 (twice timesByTwo 5))

forceLetHappy = TestCase (assertEqual "forceLet 3.0 3.0 2.0" (1.50075 * (10 ^^ (-10))) (forceLet 3.0 3.0 2.0))

forceWhereHappy = TestCase (assertEqual "forceWhere 3.0 3.0 2.0" (1.50075 * (10 ^^ (-10))) (forceWhere 3.0 3.0 2.0))

divisibleByThreeHappy = TestCase (assertEqual "divisibleByThree" [3, 6, 9, 12, 15, 18, 21, 24, 27, 30] divisibleByThree)

trianglesHappy = TestCase (assertEqual "triangles" [1, 3, 6, 10] (triangles 4))

primesHappy = TestCase (assertEqual "primes" [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31] (primes 31))

nestedHappy = TestCase (assertEqual "nested" [1, 2, 3, 4, 5, 6] (flatten [[1, 2], [3, 4], [5, 6]]))

tests =
  TestList
    [ max2Happy,
      max3Happy,
      fHappy,
      gHappy,
      twiceHappy,
      forceLetHappy,
      forceWhereHappy,
      divisibleByThreeHappy,
      trianglesHappy,
      primesHappy,
      nestedHappy
    ]

main = runTestTT tests
