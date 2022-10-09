module Exercises.Two.IO
  ( echoCaps,
    echoFile,
    calculator,
    infiniteAppend,
  )
where

import Data.Char (toUpper)
import Data.Foldable (forM_)
import System.Exit (exitFailure)

-- 1.
echoCaps :: IO ()
echoCaps = do
  text <- getLine
  let capsText = map toUpper text
  print capsText

-- 2.
echoFile :: FilePath -> IO ()
echoFile path = do
  file <- readFile path
  let fileLines = lines file
  forM_ fileLines print

-- 3.
input :: String -> IO String
input to_print = do
  putStrLn to_print
  getLine

getIOInt :: IO Int
getIOInt = do
  str <- input "Please Enter A Number: "
  -- TODO: Ask question about why this only works with the explicit return
  return (read str)

check :: String -> Bool
check operator = case operator of
  x | x `elem` ["+", "-", "*"] -> True
  _ -> False

doOperation :: String -> Int -> Int -> Int
doOperation operator x y = case operator of
  "+" -> x + y
  "-" -> x - y
  "*" -> x * y
  _ -> x + y

calculator :: IO ()
calculator = do
  operator <- input "Please Enter A Mathmatical Operator (+, -, *): "
  if check operator
    then do
      let (ioInt1, ioInt2) = (getIOInt, getIOInt)
      int1 <- ioInt1
      int2 <- ioInt2

      let result = doOperation operator int1 int2

      putStrLn ("Result: " ++ show result)
    else exitFailure

-- 4.
appendToFile :: FilePath -> IO b
appendToFile file = do
  lineToAppend <- input "Line To Append: "
  appendFile file lineToAppend
  appendToFile file

infiniteAppend :: IO ()
infiniteAppend = do
  path <- input "Please Enter In A File To Append To: "
  appendToFile path

main :: IO ()
main = do
  echoCaps

  filename <- getLine
  echoFile filename

  calculator
  infiniteAppend
