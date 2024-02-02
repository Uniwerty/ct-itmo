{-|
Module: HW6.T1
Description: A concurrent hash table module.
-}
module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy.STM (MonadSTM, TArray, TVar, modifyTVar, newTVar, readTVar)
import Control.Monad (foldM, void, when)
import Control.Monad.Conc.Class
import Data.Array.Base (getNumElements)
import Data.Array.MArray
import Data.Hashable

-- | The initial capacity of buckets array
initCapacity :: Int
initCapacity = 16

-- | The factor to use to resize the table
loadFactor :: Double
loadFactor = 0.75

-- | An array of key-value associations
type Bucket k v = [(k, v)]
-- | An array of buckets of a hash table
type BucketsArray stm k v = TArray stm Int (Bucket k v)

-- | A concurrent hash table
data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

-- | Constructs a new empty table
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = do
  array <- atomically $ newArray (0, initCapacity) []
  buckets <- newTVarConc array
  size <- newTVarConc 0
  return $ CHT { chtBuckets = buckets
               , chtSize = size
               }

-- | Gets the value by the given key from the given table,
-- or Nothing if the given key is not present in the table
getCHT
   :: ( MonadConc m
      , Eq k
      , Hashable k
      )
   => k
   -> CHT (STM m) k v
   -> m (Maybe v)
getCHT key table = atomically $ do
  buckets <- readTVar $ chtBuckets table
  capacity <- getNumElements buckets
  bucket <- readArray buckets (hash key `mod` capacity)
  return $ findKey bucket key

-- | Puts the given value by the given key to the given table
putCHT
   :: ( MonadConc m
      , Eq k
      , Hashable k
      )
   => k
   -> v
   -> CHT (STM m) k v
   -> m ()
putCHT key value table = atomically $ do
  buckets <- readTVar $ chtBuckets table
  capacity <- getNumElements buckets
  let position = hash key `mod` capacity
  bucket <- readArray buckets position
  modifiedBucket <- putKey table bucket key value
  writeArray buckets position modifiedBucket
  ensureCapacity table capacity

-- | Gets the current size of the given table
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT table = readTVarConc $ chtSize table

-- | Finds the value by the given key in the given bucket
findKey :: Eq k => Bucket k v -> k -> Maybe v
findKey ((k, v) : _) key
  | k == key = Just v
findKey (_ : xs) key = findKey xs key
findKey [] _ = Nothing

-- | Puts the given value by the given key to the given bucket
putKey :: (MonadSTM m, Eq k) => CHT m k v -> Bucket k v -> k -> v -> m (Bucket k v)
putKey table (pair@(k, _) : xs) key value
  | k == key = return $ (key, value) : xs
  | otherwise = do
    bucket <- putKey table xs key value
    return $ pair : bucket
putKey table [] key value = do
  modifyTVar (chtSize table) (+1)
  return [(key, value)]

-- | Resizes the table capacity to twice and transforms all the buckets
-- if the current size is greater than the current capacity multiplied by the load factor
ensureCapacity
    :: ( MonadSTM m
       , Hashable k
       )
    => CHT m k v
    -> Int
    -> m ()
ensureCapacity table capacity = do
  size <- readTVar $ chtSize table
  when (fromInteger (toInteger size) >= fromInteger (toInteger capacity) * loadFactor) $
   void $ do
     buckets <- readTVar $ chtBuckets table
     bArray <- getElems buckets
     transformed <- transform bArray (capacity * 2)
     newBuckets <- newTVar transformed
     return $ table { chtBuckets = newBuckets }

-- | Transforms the buckets array to the array with the new capacity
transform
    :: ( MonadSTM m
       , Hashable k
       )
    => [Bucket k v]
    -> Int
    -> m (BucketsArray m k v)
transform buckets newCapacity = do
  newBuckets <- newArray (0, newCapacity) []
  foldM (transformBucket newCapacity) newBuckets buckets

-- | Transforms the given bucket according to the new capacity
transformBucket
    :: ( MonadSTM m
       , Hashable k
       )
    => Int
    -> BucketsArray m k v
    -> Bucket k v
    -> m (BucketsArray m k v)
transformBucket newCapacity buckets ((key, value) : xs) = do
  let position = hash key `mod` newCapacity
  bucket <- readArray buckets position
  writeArray buckets position ((key, value) : bucket)
  transformBucket newCapacity buckets xs
transformBucket _ buckets [] = return buckets
