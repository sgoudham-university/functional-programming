module Exercises.One.Functions
  ( max2,
    max3,
    f,
    g,
    twice,
    forceLet,
    forceWhere,
  )
where

-- FUNCTIONS --
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

-- 6.
forceLet :: Fractional a => a -> a -> a -> a
forceLet m1 m2 d =
  let g = 6.67 * (10 ^^ (-11))
      top = m1 * m2
      bottom = d * d
   in g * (top / bottom)

forceWhere :: Fractional a => a -> a -> a -> a
forceWhere m1 m2 d = g * ((m1 * m2) / (d * d))
  where
    g = 6.67 * (10 ^^ (-11))
