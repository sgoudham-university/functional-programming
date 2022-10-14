module Exercises.Three.Recursion
  ( isAscending,
    myTake,
    dropOdds,
    myIntersperse,
    myReverseRec,
    myReverseFold,
  )
where

isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [_] = True
isAscending (first : second : rest) = first < second && isAscending rest

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake num (x : xs) = x : myTake (num - 1) xs

dropOdds :: [a] -> [a]
dropOdds _ = undefined

myIntersperse :: [a] -> a -> [a]
myIntersperse _ _ = undefined

myReverseRec :: [a] -> [a]
myReverseRec _ = undefined

myReverseFold :: [a] -> [a]
myReverseFold _ = undefined
