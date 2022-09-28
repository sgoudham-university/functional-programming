import Control.Exception (assert)

-- Functions
-- 1.
largestNum x y = if x >= y then x else y

-- 2.
largestThreeNum x y z = largestNum (largestNum x y) z

-- 3.
f :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
f f1 f2 = \x -> f2 (f1 x)

-- 4.
g :: (Int -> Bool) -> (Bool -> String) -> Int -> String
g f1 f2 int = f2 (f1 int)

-- 5.
twice :: (Int -> Int) -> Int -> Int
twice f1 int = f1 (f1 int)

-- HELPERS --
stringToBool s = length s == 5

intToBool i = i == 10

timesByTwo x = x * 2

-- main
main = do
  putStrLn (assert (largestNum 10 100 == 100) "(largestNum 10 100 == 100) Success!")
  putStrLn ("1. largestNum(10 100): " ++ show (largestNum 10 100))
  putStrLn ("2. largestThreeNum(10 100 1000): " ++ show (largestThreeNum 10 100 1000))
  putStrLn ("3. Expected 'True': " ++ show (f show stringToBool 10000))
  putStrLn ("3. Expected 'False': " ++ show (f show stringToBool 10))
  putStrLn ("4. Expected 'True': " ++ g intToBool show 10)
  putStrLn ("4. Expected 'False': " ++ g intToBool show (-1))
  putStrLn ("5. Expected " ++ g intToBool show (-1))
