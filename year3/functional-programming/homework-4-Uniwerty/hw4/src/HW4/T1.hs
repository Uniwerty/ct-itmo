{-|
Module: HW4.T1
Description: A module with ExceptState type, functions and instances for it
and expression evaluating function with division by zero handling.
-}
module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad (ap)
import HW4.Types

-- | An annotated state or the specified error
newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

-- | Applies the given function to the given 'ExceptState' to get a new 'ExceptState'
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f state = ES $ \s ->
    case runES state s of
      Error e           -> Error e
      Success (a :# s2) -> Success $ f a :# s2

-- | Wraps the given value to 'ExceptState'
wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success $ a :# s

-- | Joins the given 'ExceptState' of 'ExceptState' to 'ExceptState'
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState state = ES $ \s ->
    case runES state s of
      Error e -> Error e
      Success (state2 :# s2) ->
          let result = runES state2 s2
          in case result of
            Error e2 -> Error e2
            _        -> result

-- | Gets 'ExceptState' modified with the given function
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success $ () :# f s

-- | Wraps the given error to 'ExceptState'
throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  stateA >>= f = joinExceptState $ fmap f stateA

data EvaluationError = DivideByZero
  deriving (Show, Eq)

-- | Evaluates the given expression to ExceptState of 'Double' value and 'EvaluationError',
-- tracing evaluations as annotation
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
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
            -> ExceptState EvaluationError [Prim Double] Double
evalBinary exprOp doubleOp left right = do
  x <- eval left
  y <- eval right
  performOperation $ exprOp x y
  return $ doubleOp x y

performOperation :: Prim Double -> ExceptState EvaluationError [Prim Double] ()
performOperation (Div _ 0) = throwExceptState DivideByZero
performOperation expr      = modifyExceptState (expr : )

evalUnary :: (Double -> Prim Double)
            -> (Double -> Double)
            -> Expr
            -> ExceptState EvaluationError [Prim Double] Double
evalUnary exprOp doubleOp value = do
  x <- eval value
  modifyExceptState (exprOp x : )
  return $ doubleOp x
