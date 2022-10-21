module Exercises.Three.AlgabraicDatatypes
  ( evalExpr,
    showExpr,
    safeHead,
    safeDiv,
    addSafeDiv,
    sumTree,
    reverseTree,
    toTree,
    fromTree,
    Tree (..),
    BinaryTree (..),
    ArithExpr (..),
  )
where

-- 1.
data ArithExpr
  = Add ArithExpr ArithExpr
  | Sub ArithExpr ArithExpr
  | Mul ArithExpr ArithExpr
  | Div ArithExpr ArithExpr
  | Value Int

-- a.
evalExpr :: ArithExpr -> Int
evalExpr (Value i) = i
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Sub a b) = evalExpr a - evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
evalExpr (Div a b) = evalExpr a `div` evalExpr b

-- 2.
parensIfNeeded :: ArithExpr -> String
parensIfNeeded (Value a) = showExpr (Value a)
parensIfNeeded b = "(" ++ showExpr b ++ ")"

showExpr :: ArithExpr -> String
showExpr (Value i) = show i
showExpr (Add a b) = parensIfNeeded a ++ " + " ++ parensIfNeeded b
showExpr (Sub a b) = parensIfNeeded a ++ " - " ++ parensIfNeeded b
showExpr (Mul a b) = parensIfNeeded a ++ " * " ++ parensIfNeeded b
showExpr (Div a b) = parensIfNeeded a ++ " / " ++ parensIfNeeded b

-- 2.
-- a.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- b.
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

-- c.
addSafeDiv :: (Int, Int) -> (Int, Int) -> Maybe Int
addSafeDiv (a, b) (c, d) =
  case safeDiv a b of
    Nothing -> Nothing
    (Just resA) -> case safeDiv c d of
      Nothing -> Nothing
      (Just resB) -> Just (resA + resB)

{-
Using do notation:
  addSaveDiv (a, b) (c, d) = do
    res1 <- safeDiv a b
    res2 <- safeDiv c d
    return $ res1 + res2

Another variation of case statement:
addSafeDiv (a1, a2) (b1, b2) = case (safeDiv a1 a2, safeDiv b1 b2) of
    (Just a, Just b) -> Just $ a + b
    _                -> Nothing
-}

-- 3.
data Tree a = Leaf | Node a [Tree a]
  deriving(Eq, Show)

-- a.
sumTree :: Tree Int -> Int
sumTree Leaf = 0
sumTree (Node a children) = a + sum (map sumTree children)

-- b.
reverseTree :: Tree a -> Tree a
reverseTree Leaf = Leaf
reverseTree (Node a list_of_trees) = Node a (map reverseTree (reverse list_of_trees))

-- 4.
data BinaryTree a = BLeaf | BNode a (BinaryTree a) (BinaryTree a)
  deriving(Eq, Show)

-- a.
toTree :: BinaryTree a -> Tree a
toTree BLeaf = Leaf
toTree (BNode a left right) = Node a [toTree left, toTree right]

-- b.
fromTree :: Tree a -> Maybe (BinaryTree a)
fromTree Leaf = Just BLeaf
fromTree (Node a [x, y]) = case fromTree x of
  Nothing -> Nothing
  Just resA -> case fromTree y of
    Nothing -> Nothing
    Just resB -> Just (BNode a resA resB)
fromTree (Node a [x]) = case fromTree x of
  Nothing -> Nothing
  Just resA -> Just (BNode a resA BLeaf)
fromTree _ = Nothing

{-
Using do notation:
  fromTree (Node a [left, right]) = do
    left' <- fromTree left
    right' <- fromTree right
    pure $ BNode a left' right'

Another variation of case statement:
  fromTree :: Tree a -> BTree a
  fromTree Leaf = Just BLeaf
  fromTree (Node a [x, y]) = case (fromTree x, fromTree y) of
      (Just x, Just y) -> Just $ BNode a x y
      _                -> Nothing
  fromTree _ = Nothing
-}

-- Solution from my friend that completely goes over the top of whats required xD
--
-- {-# language DeriveFoldable #-}
-- data Tree a = Leaf | Node a [Tree a]
--   deriving Foldable

-- sumTree :: Tree Int -> Int
-- sumTree = foldl (+) 0

main = do
  print (sumTree (Node 10 [Leaf]))
