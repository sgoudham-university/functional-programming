module Exercises.Five.LazyEvaluation
  ( ackermann,
    butIfZero,
    main,
  )
where

ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

-- 1.
butIfZero :: Integer -> Integer -> Integer
butIfZero x y = if x /= 0 then x else y

-- 2.a.
-- f :: String -> String -> Integer -> Integer
-- f :: Eq a => a -> a -> b -> b

-- 2.b
-- Always Evaluated: x
-- Conditionally Evaluted: y, z

-- 2.c
-- f "hello" "c++" (ackermann 4 2)
-- I wouldn't expect the evaluation to take a long time as y is not equal to haskell
-- therefore, z is not conditionally evaluated

-- 2.d
-- With the bang operator, ackerman would get evaluated at the call site
-- therefore, the function will take a long time to evaluate/execute

-- 2.e
-- foldl vs foldl', foldl is strict, foldl' is lazy
-- It's useful to strictly evaluate to avoid the possibility of overflowing the stack

main :: IO ()
main = do
  print (1 + 1 `butIfZero` ackermann 4 2)

-- Takes a very long time
-- print (ackermann 4 2 `butIfZero` 1 + 1)
