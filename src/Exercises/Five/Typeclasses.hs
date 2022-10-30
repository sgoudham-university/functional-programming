module Exercises.Five.Typeclasses
  ( Vehicle,
    main,
  )
where

-- 1.
data Vehicle = Bicycle | Car | Bus
  deriving (Show)

-- 2.
data Vehicle' = Bicycle' | Car' | Bus'
  deriving (Show)

-- 3.
data Vehicle'' = Bicycle'' | Car'' | Bus''
  deriving (Show, Eq)

-- 4.
numWheels :: Vehicle'' -> Int
numWheels Bus'' = 4
numWheels Car'' = 4
numWheels Bicycle'' = 2

-- 5.
instance Ord Vehicle'' where
  compare x y = compare (numWheels x) (numWheels y)

-- 6.
-- TODO

main :: IO ()
main = do
  print Bus

  print (Bus'' == Bus'')
  print (Bus'' == Car'')

  print (numWheels Bus'')
  print (numWheels Car'')
  print (numWheels Bicycle'')

  print (Bus'' == Car'')
  print (Bicycle'' < Car'')
  print (Bicycle'' > Car'')
