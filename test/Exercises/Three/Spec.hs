module Exercises.Three.Spec (threeTests) where

import Exercises.Three.Recursion
import Test.HUnit

isAscendingHappy :: Test
isAscendingHappy = TestCase (assertEqual "isAscendingHappy" True (isAscending [-1, 0, 1, 2, 3, 4]))

isAscendingSad :: Test
isAscendingSad = TestCase (assertEqual "isAscendingSad" False (isAscending [1, 2, 4, 0]))

myTakeHappy :: Test
myTakeHappy = TestCase (assertEqual "myTakeHappy" [1, 2, 3] (myTake 3 [1, 2, 3, 4]))

myTakeNumOverflow :: Test
myTakeNumOverflow = TestCase (assertEqual "myTakeNumOverflow" [1, 2, 4, 0] (myTake 10 [1, 2, 4, 0]))

threeTests :: Test
threeTests = TestList [isAscendingHappy, isAscendingSad, myTakeHappy, myTakeNumOverflow]
