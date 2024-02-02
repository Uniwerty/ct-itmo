{-|
Module: Data.Grid
Description: A two-dimensional grid module.
-}
module Data.Grid
  ( Grid (..)
  , gWrite
  , gLeft
  , gRight
  , gUp
  , gDown
  ) where

import Control.Comonad (Comonad (..))
import Data.ListZipper

-- | A two-dimensional grid data structure
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f g = Grid $ LZ (map (fmap f) ls) (fmap f x) (map (fmap f) rs)
    where (LZ ls x rs) = unGrid g

instance Comonad Grid where
  extract = extract . extract . unGrid
  duplicate = Grid . fmap gHorizontal . gVertical

-- | Move one step horizontally
gHorizontal :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight

-- | Move one step vertically
gVertical :: Grid a -> ListZipper (Grid a)
gVertical = lGenerator gUp gDown

-- | Move one step up
gUp :: Grid a -> Grid a
gUp (Grid g) = Grid (lLeft g)

-- | Move one step down
gDown :: Grid a -> Grid a
gDown (Grid g) = Grid (lRight g)

-- | Move one step left
gLeft :: Grid a -> Grid a
gLeft (Grid g) = Grid (fmap lLeft g)

-- | Move one step right
gRight :: Grid a -> Grid a
gRight (Grid g) = Grid (fmap lRight g)

-- | Writes the focus element
gWrite :: a -> Grid a -> Grid a
gWrite x (Grid g) = Grid $ lWrite newLine g
  where oldLine = extract g
        newLine = lWrite x oldLine