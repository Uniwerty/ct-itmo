{-|
Module: HW0.T2
Description: A module with Not type,
double negation derivation and triple negation reduction.
-}
module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

-- | A negation of type
type Not a = a -> Void

-- | Derives the double negation
doubleNeg :: a -> Not (Not a)
doubleNeg a = axiom9 (axiom1 a) id

-- | Reduces the triple negation
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg na = axiom9 doubleNeg (const na)

-- | Implements the first axiom scheme
axiom1 :: a -> b -> a
axiom1 a _ = a

-- | Implements the ninth axiom scheme
axiom9 :: (a -> b) -> (a -> Not b) -> Not a
axiom9 f g a = g a (f a)
