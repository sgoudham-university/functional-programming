module Exercises.Exam.AdditionalExercises () where

import Control.Monad
import Data.Char
import Text.Parsec
import Text.Parsec.String

-- [[Functors and Applicative Functors]]

-- 1(a)

data BoxedValue a
  = IntBox Int
  | StringBox String
  | CustomBox a

-- Functors are containers, they allow us to apply operations on these containers without the effort required to "lift" them up,
-- apply them, and put it back in the container.

-- 1(b)

instance Functor BoxedValue where
  fmap _ (IntBox a) = IntBox a
  fmap _ (StringBox b) = StringBox b
  fmap f (CustomBox a) = CustomBox (f a)

-- 1(c)
-- We should prove that the `identity` and `composition` law's hold

-- fmap id (IntBox x) = IntBox x
-- fmap id (StringBox x) = StringBox x
-- fmap id (CustomBox x) = CustomBox x

-- 1(d)
-- It is important that we satisfy the functor laws because bad functors change the shape of the data and we don't want that.
-- If an instance does not follow the functor laws properly, you may find that data is returned in the wrong shape and
-- the program may even throw an error

-- 2
parseSentence :: Parser String
parseSentence = many anyChar

parseSentenceUpper :: Parser String
parseSentenceUpper = Prelude.fmap (map toUpper) parseSentence

-- 3
-- The purpose of an applicative functor is so that the normal functors can be generalised for any function in a context to
-- be applied to any argument in context

-- 4
-- TODO
-- Given a function f :: a -> b -> c, you could do f a b c = a <*> b <*> c to sequence potentially failing computations

-- [[Monads]]
-- 1
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- fmap :: Functor f -> (a -> b) -> f a -> f b
-- join :: Monad m => m (m a) -> m a
hammyBind :: Monad m => m a -> (a -> m b) -> m b
hammyBind m f = join (fmap f m)

-- 2
hammyJoin :: Monad m => m (m a) -> m a
hammyJoin m = hammyBind m id

-- 3
ap :: Monad m => m (a -> b) -> m a -> m b
ap f m = join $ fmap (\f -> fmap f m) f

-- 4(a)
safeDiv :: Int -> Int -> Maybe Int -- which will return Just (div x y)
safeDiv a b = Just div a b
safeDiv _ 0 = Nothing

divTwiceDo :: Int -> Int -> Maybe Int
divTwiceDo a b = do
  res <- safeDiv a b
  safeDiv res b

divTwiceBind :: Int -> Int -> Maybe Int
divTwiceBind a b = safeDiv a b >>= safeDiv b

-- 4(b)
divAndAddDo :: Int -> Int -> Int -> Maybe Int
divAndAddDo a b c = do
  res <- safeDiv a b
  Just $ res + c

divAndAddBind :: Int -> Int -> Int -> Maybe Int
divAndAddBind a b c = safeDiv a b >>= (\r -> Just $ r + c)

-- 5
data BinOp = Add | Sub | Mul

data Value = VInt Int | VString String | VUnit

type Store = [(Variable, Int)]

type Variable = String

data Expr
  = ESet Variable Expr
  | EGet Variable
  | EString String
  | EUnit
  | EInt Int
  | EIntToString Expr
  | EOp BinOp Expr Expr
  | ESeq Expr Expr

type Computation a = State Store a

-- 5(a)
unwrapInt :: Value -> Int
unwrapInt (VInt a) = a
unwrapInt _ = error "*jeremy's voice* oh no!!!"

-- 5(b)
-- lookupState :: Variable -> Computation Int
-- TODO: Come back to this later

-- [[Failure Handling]]

-- 1.
-- Advantage: If the constraint imposed by the data has already been taken care of, partial functions allow for less verbosity
-- Disadvantage: Partial functions can result in nasty runtime errors and/or bugs when different/unvalidated data is passed through the system

-- 2.
wrap :: a -> IO (Either String a)
wrap a = do
  something <- error "*jeremy's voice* oh no!!!"
  case something of
    Left err -> return $ Left err
    Right y -> return $ Right y

-- 3.
-- Advantage: This results in more robust error handling, making sure that IO computations do not crash the program
-- Disadvantage: It's difficult to pinpoint exactly where the error came from as the original error stacktrace is lost

-- 4.
-- Either allows us to specify **why** the computation has failed, the Maybe monad does not let us do this.

-- 5.
data Error = DivByZero | OutOfBounds deriving (Show)

type Computation' a = Either Error a

-- Furthermore, say we have the following functions
maxInt :: Computation' Int
maxInt = Right 20

safeDiv' :: Int -> Int -> Computation' Int
safeDiv' _ 0 = Left DivByZero
safeDiv' a b = Right $ a `div` b

safeAdd :: Int -> Int -> Computation' Int
safeAdd a b = result
 where
  comp = Right $ a + b
  result
    | comp > maxInt = Left OutOfBounds
    | otherwise = comp

safeSub :: Int -> Int -> Computation' Int
safeSub a b = result
 where
  comp = Right $ a - b
  result
    | comp > maxInt = Left OutOfBounds
    | otherwise = comp

-- 5(a)
-- The purpose of the deriving (Show) is so that the values can be automatically converted to a string without the user needing
-- to manually implement the Show typeclass

-- 5(b)
comp :: Computation' Int
comp = (maxInt `div` 2 + 5) `div` 10

-- 5(c)
run :: Computation' a -> IO ()
run comp = do
  case comp of
    Left err -> print $ "*jeremy's voice* ohhh nooo!: " ++ show err
    Right x -> print x

-- 5(d)
runWithDefault :: a -> Computation' a -> a
runWithDefault a b = case b of
  Left _ -> a
  Right x -> x

-- 6.
-- Using Control.Except is advantageous because it includes convenience functions for throwing and catching exceptions.

-- [[Referential Transparency & Equational Reasoning]]

-- 1.
-- Referential transparency is useful because it guarantees that there are no side-effects, hence great for parallelism
-- It is also helpful for reasoning about the design and structure of programs

-- 2.
-- The program would not be referentially transparent if the functions returns a random number.
-- However, this could be recovered if the random number was generated off a seed, allowing for the function to return the same "random" number each time

-- 3.
-- data BinaryTree a = Leaf | Node x (BinaryTree a) (BinaryTree a)
-- TODO: Sketch structural induction proof

-- 4.
-- proplen = size_of_binary_tree `mod` 2 /= 0

-- 5.
-- Property based testing relies on referential transparency because it would be difficult to reason about the correctness of a program based on its inputs
-- and outputs. For example, there may be global mutable state without referential transparency which makes it very difficult to test consistently
