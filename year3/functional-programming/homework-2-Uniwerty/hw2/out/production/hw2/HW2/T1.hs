{-|
Module: HW2.T1
Description: Binary tree fold module
-}
module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

-- | Binary tree
data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

-- | 'Tree' right fold function
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc Leaf = acc
tfoldr f acc (Branch _ left value right) = let rightAcc = tfoldr f acc right
                                               midAcc   = f value rightAcc
                                           in  tfoldr f midAcc left

