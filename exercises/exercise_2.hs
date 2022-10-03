import Test.HUnit

-- BOOLEANS, COMPARISONS, TUPLES, AND MORE lISTS --
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
-- TODO

-- IO --
-- TODO

isAscending4Happy = TestCase (assertEqual "isAscending4" True (isAscending4 1 2 3 4))
swapHappy = TestCase (assertEqual "swap" ("Cat", "Ppuccin") (swap ("Ppuccin", "Cat")))

rotateHappy = TestCase (assertEqual "rotate" ("Cat", "Winston", "Ppuccin") (rotate ("Winston", "Ppuccin", "Cat")))

myCurryHappy = TestCase (assertEqual "myCurry" 20 (myCurry (\(x, y) -> x + y) 10 10))

myUncurryHappy = TestCase (assertEqual "myUncurry" 20 (myUncurry (\x y -> x + y) (10, 10)))

myFlipHappy = TestCase (assertEqual "myFlip" (-10) (myFlip (\x y -> x - y) 20 10))

mapPairHappy = TestCase (assertEqual "mapPair" (16, 32) (mapPair (\x -> x * 2) (\y -> y `div` 2) (8, 64)))

mapPair2Happy = TestCase (assertEqual "mapPair2" (16, 32) (mapPair2 (\(x, y) -> (x * 2, y `div` 2)) (8, 64)))

fHappy = TestCase (assertEqual "f" 120 (f (20, 20) (\x -> x * 3) (\x -> x * 3) (\(x, y) -> x + y)))

tests = TestList [isAscending4Happy, swapHappy, rotateHappy, myCurryHappy, myUncurryHappy, myFlipHappy, mapPairHappy, mapPair2Happy, fHappy]

main = runTestTT tests
