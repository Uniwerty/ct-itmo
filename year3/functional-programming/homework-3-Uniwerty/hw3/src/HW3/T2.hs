{-# LANGUAGE TupleSections #-}

{-|
Module: HW3.T2
Description: A module with wrap and dist functions for types defined in HW3.T1 module
-}
module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1

-- | Gets 'Option' of pair from pair of 'Option's
distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        = None
distOption (_, None)        = None
distOption (Some a, Some b) = Some (a, b)

-- | Wraps the given value to 'Option'
wrapOption :: a -> Option a
wrapOption a = Some a

-- | Gets 'Pair' of pair from pair of 'Pair's
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

-- | Wraps the given value to 'Pair'
wrapPair :: a -> Pair a
wrapPair a = P a a

-- | Gets 'Quad' of pair from pair of 'Quad's
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

-- | Wraps the given value to 'Quad'
wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

-- | Gets 'Annotated' of pair from pair of 'Annotated' values.
-- Annotations of two values are combined with (<>).
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# ea, b :# eb) = (a, b) :# ea <> eb

-- | Wraps the given value to 'Annotated' with an empty annotation
wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

-- | Gets 'Except' of pair from pair of 'Except' values
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

-- | Wraps the given value to 'Except' with that value
wrapExcept :: a -> Except e a
wrapExcept a = Success a

-- | Gets 'Option' of pair from pair of 'Option's.
-- The priority of the new pair is the highest priority of two values.
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, High b)     = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)

-- | Wraps the given value to 'Prioritised' with low priority
wrapPrioritised :: a -> Prioritised a
wrapPrioritised a = Low a

-- | Gets 'Stream' of pairs from pair of 'Stream's
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> streamA, b :> streamB) = (a, b) :> distStream (streamA, streamB)

-- | Wraps the given value to 'Stream' of this value
wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

-- | Gets 'List' of pairs from pair of 'List's.
-- The resulting 'List' is the cartesian product of two 'List's.
distList :: (List a, List b) -> List (a, b)
distList (Nil, _)              = Nil
distList (_, Nil)              = Nil
distList ((a :. tailA), listB) = concatList (mapList (a,) listB) (distList (tailA, listB))

concatList :: List a -> List a -> List a
concatList Nil list          = list
concatList (a :. tailA) list = a :. concatList tailA list

-- | Wraps the given value to 'List'
wrapList :: a -> List a
wrapList a = a :. Nil

-- | Gets 'Fun' of pair from pair of 'Fun's
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\i -> (f i, g i))

-- | Wraps the given value to 'Fun' to that value
wrapFun :: a -> Fun i a
wrapFun a = F (\_ -> a)
