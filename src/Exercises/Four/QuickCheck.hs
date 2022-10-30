module Exercises.Four.QuickCheck
  ( myReverseRec,
    reversePropLen,
    isSorted,
    sortPropLen,
    main
  )
where

import Data.List (sort)
import Test.QuickCheck (verboseCheck)

-- 1.
myReverseRec :: [a] -> [a]
myReverseRec [] = []
myReverseRec (x : xs) = myReverseRec xs ++ [x]

reversePropLen :: Eq a => [a] -> Bool
reversePropLen list = myReverseRec (myReverseRec list) == list

-- 2.
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (first : second : rest) = first <= second && isSorted rest

sortPropLen :: [Int] -> Bool
sortPropLen list = isSorted (sort list)

main :: IO ()
main = do
  verboseCheck (reversePropLen :: [Int] -> Bool)
  putStrLn "poggers"
  verboseCheck sortPropLen
  putStrLn "poggers"
