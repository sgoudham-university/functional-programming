import Test.HUnit

-- Functions
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

force m1 m2 d =
  let g = 6.67 * (10 ^^ (-11))
      top = m1 * m2
      bottom = d * d
   in g * (top / bottom)

-- HELPERS --
stringToBool s = length s == 5

intToBool i = i == 10

timesByTwo x = x * 2

-- TESTS
max2Happy = TestCase (assertEqual "max2 10 100 == 100" 100 (max2 10 100))

max3Happy = TestCase (assertEqual "max3 10 100 1000" 1000 (max3 10 100 1000))

fHappy = TestCase (assertEqual "f show stringToBool 10000" True (f show stringToBool 10000))

gHappy = TestCase (assertEqual "g intToBool show 10" "True" (g intToBool show 10))

twiceHappy = TestCase (assertEqual "twice timesByTwo 5" 20 (twice timesByTwo 5))

forceHappy = TestCase (assertEqual "force 3.0 3.0 2.0" (1.50075 * (10 ^^ (-10))) (force 3.0 3.0 2.0))

tests = TestList [max2Happy, max3Happy, fHappy, gHappy, twiceHappy, forceHappy]

main = runTestTT tests
