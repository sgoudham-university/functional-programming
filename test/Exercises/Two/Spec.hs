module Exercises.Two.Spec (twoTests) where

import Exercises.Two.Booleans
import Test.HUnit

isAscending4Happy :: Test
isAscending4Happy = TestCase (assertEqual "isAscending4" True (isAscending4 1 2 3 4))

swapHappy :: Test
swapHappy = TestCase (assertEqual "swap" ("Cat", "Ppuccin") (swap ("Ppuccin", "Cat")))

rotateHappy :: Test
rotateHappy = TestCase (assertEqual "rotate" ("Cat", "Winston", "Ppuccin") (rotate ("Winston", "Ppuccin", "Cat")))

myCurryHappy :: Test
myCurryHappy = TestCase (assertEqual "myCurry" 20 (myCurry (\(x, y) -> x + y) 10 10))

myUncurryHappy :: Test
myUncurryHappy = TestCase (assertEqual "myUncurry" 20 (myUncurry (\x y -> x + y) (10, 10)))

myFlipHappy :: Test
myFlipHappy = TestCase (assertEqual "myFlip" (-10) (myFlip (\x y -> x - y) 20 10))

mapPairHappy :: Test
mapPairHappy = TestCase (assertEqual "mapPair" (16, 32) (mapPair (\x -> x * 2) (\y -> y `div` 2) (8, 64)))

mapPair2Happy :: Test
mapPair2Happy = TestCase (assertEqual "mapPair2" (16, 32) (mapPair2 (\(x, y) -> (x * 2, y `div` 2)) (8, 64)))

fHappy :: Test
fHappy = TestCase (assertEqual "f" 120 (f (20, 20) (\x -> x * 3) (\x -> x * 3) (\(x, y) -> x + y)))

twoTests :: Test
twoTests = TestList [isAscending4Happy, swapHappy, rotateHappy, myCurryHappy, myUncurryHappy, myFlipHappy, mapPairHappy, mapPair2Happy, fHappy]
