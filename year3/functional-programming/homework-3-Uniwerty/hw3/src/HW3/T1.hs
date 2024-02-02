{-# LANGUAGE LambdaCase #-}

{-|
Module: HW3.T1
Description: A module with some data types and map functions for them
-}
module HW3.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

-- | An optional value
data Option a = None | Some a
  deriving (Show, Eq)

-- | Maps the given function to the function on 'Option'
mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f = \case
    None -> None
    Some a -> Some $ f a

-- | A pair of values of the same type
data Pair a = P a a
  deriving (Show, Eq)

-- | Maps the given function to the function on 'Pair'
mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f = \(P a1 a2) -> P (f a1) (f a2)

-- | Four values of the same type
data Quad a = Q a a a a
  deriving (Show, Eq)

-- | Maps the given function to the function on 'Quad'
mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f = \(Q a1 a2 a3 a4) -> Q (f a1) (f a2) (f a3) (f a4)

-- | A value annotated with another value
data Annotated e a = a :# e
  deriving (Show, Eq)

infix 0 :#

-- | Maps the given function to the function on 'Annotated'
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f = \(a :# e) -> f a :# e

-- | A message containing an error or a value
data Except e a = Error e | Success a
  deriving (Show, Eq)

-- | Maps the given function to the function on 'Except'
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f = \case
    Error e -> Error e
    Success a -> Success $ f a

-- | A value prioritised from low to high
data Prioritised a = Low a | Medium a | High a
  deriving (Show, Eq)

-- | Maps the given function to the function on 'Prioritised'
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f = \case
    Low a -> Low $ f a
    Medium a -> Medium $ f a
    High a -> High $ f a

-- | A stream of values
data Stream a = a :> Stream a
  deriving (Show, Eq)

infixr 5 :>

-- | Maps the given function to the function on 'Stream'
mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f = \(a :> stream) -> f a :> mapStream f stream

-- | A list of values
data List a = Nil | a :. List a
  deriving (Show, Eq)

infixr 5 :.

-- | Maps the given function to the function on 'List'
mapList :: (a -> b) -> (List a -> List b)
mapList f = \case
    Nil -> Nil
    a :. list -> f a :. mapList f list

-- | A unary function
data Fun i a = F (i -> a)

-- | Maps the given function to the function on 'Fun'
mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f = \(F g) -> F $ f . g

-- | A binary tree
data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving (Show, Eq)

-- | Maps the given function to the function on 'Tree'
mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f = \case
    Leaf -> Leaf
    Branch left a right -> Branch (mapTree f left) (f a) (mapTree f right)
