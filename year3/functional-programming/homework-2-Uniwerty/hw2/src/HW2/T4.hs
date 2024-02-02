{-|
Module: HW2.T4
Description: Module with some types and instances
-}
module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

-- | Non-empty list type
data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show, Eq)

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last x <> list    = x :+ list
  (x :+ xs) <> list = x :+ (xs <> list)

-- | Optional values pair type
data Inclusive a b = This a | That b | Both a b
  deriving (Show, Eq)

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This x <> This y     = This (x <> y)
  That x <> That y     = That (x <> y)
  This x <> That y     = Both x y
  That x <> This y     = Both y x
  Both x y <> This z   = Both (x <> z) y
  This x <> Both y z   = Both (x <> y) z
  Both x y <> That z   = Both x (y <> z)
  That x <> Both y z   = Both y (x <> z)
  Both x y <> Both w z = Both (x <> w) (y <> z)

-- | Dot-delimited string type
newtype DotString = DS String
  deriving (Show, Eq)

instance Semigroup DotString where
  DS "" <> dString = dString
  dString <> DS "" = dString
  DS (x:[]) <> DS string = DS (x:'.':string)
  DS (x:xs) <> dString = let DS sTail = DS xs <> dString
                         in DS (x:sTail)

instance Monoid DotString where
  mempty = DS ""

-- | Unary operator function type
newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f <> F g = F (f . g)

instance Monoid (Fun a) where
  mempty = F id
