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
dropOdds [] = []
dropOdds (x : xs) =
  if even (length xs)
    then x : dropOdds xs
    else dropOdds xs

myIntersperse :: [a] -> a -> [a]
myIntersperse [] _ = []
myIntersperse (x : xs) num = x : num : myIntersperse xs num

myReverseRec :: [a] -> [a]
myReverseRec [] = []
myReverseRec (x : xs) = myReverseRec xs ++ [x]

myReverseFold :: [a] -> [a]
myReverseFold lst = foldl (\x y -> y : x) [] lst
