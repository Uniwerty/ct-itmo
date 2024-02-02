{-|
Module: HW0.T4
Description: A module with repeat, map, fibonacci and factorial functions
implemented using fix combinator
-}
module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

-- | Repeats the given element to the infinite list
repeat' :: a -> [a]
repeat' x = fix (x:)

-- | Maps the given list with the given function
map' :: (a -> b) -> [a] -> [b]
map' f = fix
  (\rec a -> case a of
    (x:xs) -> f x : rec xs
    _      -> [])

-- | Calculates the fibonacci number with the given number
fib :: Natural -> Natural
fib num = fibSeq (num, 1, 0)
  where fibSeq = fix (\rec (n, f1, f2) -> if n == 0 then f2 else rec (n - 1, f1 + f2, f1))

-- | Calculates the factorial of the given number
fac :: Natural -> Natural
fac = fix (\rec n -> if n == 0 then 1 else n * rec (n - 1))
