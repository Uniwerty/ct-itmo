module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus n Z = n
nplus n (S m) = S (nplus n m)

nmult :: N -> N -> N
nmult n Z = Z
nmult n (S m) = nplus (nmult n m) n

nsub :: N -> N -> Maybe N
nsub Z m = Nothing
nsub n Z = Just n
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z n = LT
ncmp n Z = GT
ncmp (S n) (S m) = ncmp n m

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S (nFromNatural (x - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = (nToNum n) + 1

nEven :: N -> Bool
nEven Z = True
nEven (S n) = not (nEven n)

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv n m = case ncmp n m of
   LT -> Z
   EQ -> S Z
   GT -> ndiv (nsub n m) m
 
nmod :: N -> N -> N
nmod n m = nsub n (nmult (ndiv n m) m)
