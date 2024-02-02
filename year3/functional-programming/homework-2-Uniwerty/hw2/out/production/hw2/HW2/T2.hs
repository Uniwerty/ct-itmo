{-|
Module: HW2.T2
Description: List splitting and joining module
-}
module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..))

-- | Split 'List' by a separator
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn sep (x:xs)
  | x == sep  = [] :| (contHead:contTail)
  | otherwise = (x:contHead) :| contTail
  where contHead :| contTail = splitOn sep xs

-- | Join 'List' by a separator
joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ ([] :| [])           = []
joinWith c ([] :| (x:xs))       = (c:joinWith c (x :| xs))
joinWith c ((x:xs) :| contTail) = (x:joinWith c (xs :| contTail))
