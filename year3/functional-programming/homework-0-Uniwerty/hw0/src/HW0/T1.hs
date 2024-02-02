{-|
Module: HW0.T1
Description: A module with Iso type, functions for it
and some associativity and distributivity functions.
-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

-- | An isomorphism between two types
data a <-> b = Iso (a -> b) (b -> a)

-- | Applies distributivity for Either of pair
distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

-- | Flips the types of an isomorphism
flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

-- | Takes the function from an isomorphism
runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

-- | An isomorphism of pair associativity
assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

-- | An isomorphism of Either associativity
assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso
  (\case
    Left a -> Left (Left a)
    Right (Left b) -> Left (Right b)
    Right (Right c) -> Right c)
  (\case
    Left (Left a) -> Left a
    Left (Right b) -> Right (Left b)
    Right c -> Right (Right c))
