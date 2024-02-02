{-|
Module: HW0.T5
Description: A module with Nat type,
addition and multiplication functions,
to Num and from Natural conversions.
-}
module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

-- | A natural number representation type
type Nat a = (a -> a) -> a -> a

-- | Zero representation
nz :: Nat a
nz _ a = a

-- | Successor function
ns :: Nat a -> Nat a
ns n f a = f (n f a)

-- | Adds two given numbers
nplus :: Nat a -> Nat a -> Nat a
nplus a b f x = b f (a f x)

-- | Multiplies two given numbers
nmult :: Nat a -> Nat a -> Nat a
nmult a b f = a (b f)

-- | Converts the natural number to its representation
nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural $ n - 1

-- | Converts the number representation to Num value
nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0
