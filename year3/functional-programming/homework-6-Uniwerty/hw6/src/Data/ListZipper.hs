{-|
Module: Data.ListZipper
Description: A list zipper module.
-}
module Data.ListZipper
  ( ListZipper (..)
  , lWrite
  , lLeft
  , lRight
  , lGenerator
  ) where

import Control.Comonad (Comonad (..))

-- | A list zipper data structure
data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = lGenerator lLeft lRight

-- | Generates a list zipper from the given element
-- and the given left and right list generator functions
lGenerator :: (a -> a) -> (a -> a) -> a -> ListZipper a
lGenerator leftF rightF x = LZ (iterateTail leftF x) x (iterateTail rightF x)

-- | Takes the tail of the infinitely iterated list
iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

-- | Move one step left
lLeft :: ListZipper a -> ListZipper a
lLeft (LZ (l : ls) x rs) = LZ ls l (x : rs)
lLeft lz                 = lz

-- | Move one step right
lRight :: ListZipper a -> ListZipper a
lRight (LZ ls x (r : rs)) = LZ (x : ls) r rs
lRight lz                 = lz

-- | Writes the focus element
lWrite :: a -> ListZipper a -> ListZipper a
lWrite x (LZ ls _ rs) = LZ ls x rs