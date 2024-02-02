{-|
Module: HW6.T2
Description: A type-level strings set module.
-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

-- | A type-level strings set
type TSet = [Symbol]

-- | Checks if the given set contains the given string
type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains name (name ': xs) = 'True
  Contains name (another ': xs) = Contains name xs
  Contains name '[] = 'False

-- | Deletes the given string from the given set
type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete name (name ': xs) = xs
  Delete name (another ': xs) = (another ': Delete name xs)
  Delete name '[] = '[]

-- | Adds the given string to the given set
type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add name (name ': xs) = name ': xs
  Add name (another ': xs) = (another ': Add name xs)
  Add name '[] = '[name]
