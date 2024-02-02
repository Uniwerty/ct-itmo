{-|
Module: HW3.T4
Description: A module with State type, functions and instances for it,
arithmetic operation and expression types and evaluating expression function.
-}
module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

-- | An annotated state
newtype State s a = S { runS :: s -> Annotated s a }

-- | Applies the given function to the given 'State' to get a new 'State'
mapState :: (a -> b) -> State s a -> State s b
mapState f state = S $ \s ->
    let a :# annotation = (runS state) s
    in (f a) :# annotation

-- | Wraps the given value to 'State'
wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

-- | Joins the given 'State' of 'State' to 'State'
joinState :: State s (State s a) -> State s a
joinState state = S $ \s ->
    let state2 :# s2 = (runS state) s
    in (runS state2) s2

-- | Gets 'State' modified with the given function
modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s


instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) stateF stateA = S $ \s ->
      let f :# s2 = (runS stateF) s
          stateB = fmap f stateA
      in (runS stateB) s2

instance Monad (State s) where
  (>>=) stateA f = S $ \s ->
      let a :# s2 = (runS stateA) s
      in (runS $ f a) s2

-- | Arithmetic operation
data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show, Eq)

-- | Arithmetic expression
data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y = Op $ Add x y
  x - y = Op $ Sub x y
  x * y = Op $ Mul x y
  abs = Op . Abs
  signum = Op . Sgn
  fromInteger = Val . fromInteger

instance Fractional Expr where
  x / y = Op $ Div x y
  fromRational = Val . fromRational

-- | Evaluates the given expression to State of 'Double' value
-- and trace of evaluations as annotation
eval :: Expr -> State [Prim Double] Double
eval (Val v)               = return v
eval (Op (Add left right)) = evalBinary Add (+) left right
eval (Op (Sub left right)) = evalBinary Sub (-) left right
eval (Op (Mul left right)) = evalBinary Mul (*) left right
eval (Op (Div left right)) = evalBinary Div (/) left right
eval (Op (Abs value))      = evalUnary Abs abs value
eval (Op (Sgn value))      = evalUnary Sgn signum value

evalBinary :: (Double -> Double -> Prim Double)
            -> (Double -> Double -> Double)
            -> Expr
            -> Expr
            -> State [Prim Double] Double
evalBinary exprOp doubleOp left right = do
  x <- eval left
  y <- eval right
  modifyState (exprOp x y : )
  return $ doubleOp x y

evalUnary :: (Double -> Prim Double)
            -> (Double -> Double)
            -> Expr
            -> State [Prim Double] Double
evalUnary exprOp doubleOp value = do
  x <- eval value
  modifyState (exprOp x : )
  return $ doubleOp x
