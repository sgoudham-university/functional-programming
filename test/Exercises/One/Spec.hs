module Exercises.One.Spec (oneTests) where

import Exercises.One.Functions
import Exercises.One.ListComprehension
import Test.HUnit

-- HELPERS --
stringToBool :: Foldable t => t a -> Bool
stringToBool s = length s == 5

intToBool :: (Eq a, Num a) => a -> Bool
intToBool i = i == 10

timesByTwo :: Num a => a -> a
timesByTwo x = x * 2

-- TESTS --
max2Happy :: Test
max2Happy = TestCase (assertEqual "max2 10 100" 100 (max2 10 100))

max3Happy :: Test
max3Happy = TestCase (assertEqual "max3 10 100 1000" 1000 (max3 10 100 1000))

fHappy :: Test
fHappy = TestCase (assertEqual "f show stringToBool 10000" True (f show stringToBool 10000))

gHappy :: Test
gHappy = TestCase (assertEqual "g intToBool show 10" "True" (g intToBool show 10))

twiceHappy :: Test
twiceHappy = TestCase (assertEqual "twice timesByTwo 5" 20 (twice timesByTwo 5))

forceLetHappy :: Test
forceLetHappy = TestCase (assertEqual "forceLet 3.0 3.0 2.0" (1.50075 * (10 ^^ (-10))) (forceLet 3.0 3.0 2.0))

forceWhereHappy :: Test
forceWhereHappy = TestCase (assertEqual "forceWhere 3.0 3.0 2.0" (1.50075 * (10 ^^ (-10))) (forceWhere 3.0 3.0 2.0))

divisibleByThreeHappy :: Test
divisibleByThreeHappy = TestCase (assertEqual "divisibleByThree" [3, 6, 9, 12, 15, 18, 21, 24, 27, 30] divisibleByThree)

trianglesHappy :: Test
trianglesHappy = TestCase (assertEqual "triangles" [1, 3, 6, 10] (triangles 4))

primesHappy :: Test
primesHappy = TestCase (assertEqual "primes" [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31] (primes 31))

nestedHappy :: Test
nestedHappy = TestCase (assertEqual "nested" [1, 2, 3, 4, 5, 6] (flatten [[1, 2], [3, 4], [5, 6]]))

oneTests :: Test
oneTests =
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
