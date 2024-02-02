{-|
Module: HW3.T3
Description: A module with join functions for some types defined in HW3.T1 module
-}
module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

-- | Joins the given 'Option' of 'Option' to 'Option'
joinOption :: Option (Option a) -> Option a
joinOption None          = None
joinOption (Some option) = option

-- | Joins the given 'Except' of 'Except' to 'Except'
joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)        = Error e
joinExcept (Success except) = except

-- | Joins the given 'Annotated' of 'Annotated' to 'Annotated'.
-- Annotations are combined with (<>).
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# e2 <> e1

-- | Joins the given 'List' of 'List's to 'List'
joinList :: List (List a) -> List a
joinList Nil                     = Nil
joinList (Nil :. tailList)       = joinList tailList
joinList ((a :. as) :. tailList) = a :. joinList (as :. tailList)

-- | Joins the given 'Fun' mapping to 'Fun' to 'Fun'
joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> let F g = f i in g i)
