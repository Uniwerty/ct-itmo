{-|
Module: HW2.T3
Description: 'Maybe' and 'Either' folds module
-}
module HW2.T3
  ( epart
  , mcat
  ) where

-- | Fold a 'List' of 'Maybe' elements
mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap fromMaybe

fromMaybe :: Monoid a => Maybe a -> a
fromMaybe Nothing  = mempty
fromMaybe (Just x) = x

-- | Fold a 'List' of 'Either' elements
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap fromEither

fromEither :: (Monoid a, Monoid b) => Either a b -> (a, b)
fromEither (Left x)  = (x, mempty)
fromEither (Right x) = (mempty, x)
