module Exercises.Four.ParserCombinators () where

import Text.Parsec

-- Simon's datatype for expressions
data BinOp = Add | Sub | Mul | Div | Pow deriving (Show, Eq)

data ArithExpr
  = Compound ArithExpr BinOp ArithExpr
  | Value Int
  deriving (Show)

-- a.
evalExpr :: ArithExpr -> Int
evalExpr (Value i) = i
evalExpr (Compound a Add b) = evalExpr a + evalExpr b
evalExpr (Compound a Sub b) = evalExpr a - evalExpr b
evalExpr (Compound a Mul b) = evalExpr a * evalExpr b
evalExpr (Compound a Div b) = evalExpr a `div` evalExpr b
evalExpr (Compound a Pow b) = evalExpr a ^ evalExpr b

numberParser :: Parsec String st Int
-- numberParser = read <$> (many $ oneOf "0123456789")
numberParser = read <$> (many1 digit)

-- numberParserBad :: Parsec String st Int
-- numberParserBad = fmap read (many1 digit)

operatorParser :: Parsec String st BinOp
operatorParser = selectOp <$> (oneOf "+-*/^")
  where
    selectOp '+' = Add
    selectOp '-' = Sub
    selectOp '*' = Mul
    selectOp '/' = Div
    selectOp '^' = Pow

expressionParser :: Parsec String st ArithExpr
expressionParser =
  between (char '(') (char ')') binaryExpressionParser
    <|> (Value <$> numberParser)

binaryExpressionParser :: Parsec String st ArithExpr
binaryExpressionParser = Compound <$> expressionParser <*> operatorParser <*> expressionParser

-- now parseTest expressionParser "(1+1)"
-- or parse expressionParser "error" "(1+(2*3))"
main :: IO ()
main = do
  parseTest expressionParser "1+2"

  text <- getLine
  let something = case parse binaryExpressionParser "" text of
        Left err -> error (show err)
        Right b -> evalExpr b
  print something
