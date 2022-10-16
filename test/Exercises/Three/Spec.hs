module Exercises.Three.Spec (threeTests) where

import Exercises.Three.AlgabraicDatatypes
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

dropOddsHappy :: Test
dropOddsHappy = TestCase (assertEqual "dropOddsHappy" [1, 3, 5] (dropOdds [1, 2, 3, 4, 5]))

myIntersperseHappy :: Test
myIntersperseHappy = TestCase (assertEqual "myIntersperseHappy" [1, 10, 2, 10, 3, 10] (myIntersperse [1, 2, 3] 10))

myReverseRecHappy :: Test
myReverseRecHappy = TestCase (assertEqual "myReverseRec" "goudham" (myReverseRec "mahduog"))

evalExprHappy :: Test
evalExprHappy = TestCase (assertEqual "evalExpr" 20 (evalExpr (Add (Value 10) (Value 10))))

showExprHappy :: Test
showExprHappy = TestCase (assertEqual "showExpr" "(2 * 4) + 5" (showExpr (Add (Mul (Value 2) (Value 4)) (Value 5))))

safeHeadHappy :: Test
safeHeadHappy = TestCase (assertEqual "safeHeadHappy" (Just 5) (safeHead [5, 10]))

safeHeadSad :: Test
safeHeadSad = TestCase (assertEqual "safeHeadSad" (Nothing :: Maybe Int) (safeHead []))

safeDivHappy :: Test
safeDivHappy = TestCase (assertEqual "safeDivHappy" (Just 5) (safeDiv 10 2))

safeDivSad :: Test
safeDivSad = TestCase (assertEqual "safeDivSad" (Nothing) (safeDiv 10 0))

addSafeDivHappy :: Test
addSafeDivHappy = TestCase (assertEqual "addSafeDivHappy" (Just 10) (addSafeDiv (10, 2) (10, 2)))

-- TODO: MORE TESTS REQUIRED

threeTests :: Test
threeTests =
  TestList
    [ isAscendingHappy,
      isAscendingSad,
      myTakeHappy,
      myTakeNumOverflow,
      dropOddsHappy,
      myIntersperseHappy,
      myReverseRecHappy,
      evalExprHappy,
      showExprHappy,
      safeHeadHappy,
      safeHeadSad,
      safeDivHappy,
      safeDivSad,
      addSafeDivHappy
    ]
