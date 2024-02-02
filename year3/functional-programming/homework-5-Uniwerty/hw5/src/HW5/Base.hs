{-# LANGUAGE DeriveGeneric #-}
module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , HiAction (..)
  , HiMonad (..)
  ) where

import Codec.Serialise
import Data.ByteString
import Data.Map.Strict
import Data.Sequence
import Data.Text
import Data.Time.Clock
import GHC.Generics

data HiFun = HiFunDiv
              | HiFunMul
              | HiFunAdd
              | HiFunSub
              | HiFunNot
              | HiFunAnd
              | HiFunOr
              | HiFunLessThan
              | HiFunGreaterThan
              | HiFunEquals
              | HiFunNotLessThan
              | HiFunNotGreaterThan
              | HiFunNotEquals
              | HiFunIf
              | HiFunLength
              | HiFunToUpper
              | HiFunToLower
              | HiFunReverse
              | HiFunTrim
              | HiFunList
              | HiFunRange
              | HiFunFold
              | HiFunPackBytes
              | HiFunUnpackBytes
              | HiFunEncodeUtf8
              | HiFunDecodeUtf8
              | HiFunZip
              | HiFunUnzip
              | HiFunSerialise
              | HiFunDeserialise
              | HiFunRead
              | HiFunWrite
              | HiFunMkDir
              | HiFunChDir
              | HiFunParseTime
              | HiFunRand
              | HiFunEcho
              | HiFunCount
              | HiFunKeys
              | HiFunValues
              | HiFunInvert
              deriving (Show, Eq, Ord, Generic)

instance Serialise HiFun

data HiValue = HiValueNumber Rational
                 | HiValueFunction HiFun
                 | HiValueBool Bool
                 | HiValueNull
                 | HiValueString Text
                 | HiValueList (Seq HiValue)
                 | HiValueBytes ByteString
                 | HiValueAction HiAction
                 | HiValueTime UTCTime
                 | HiValueDict (Map HiValue HiValue)
                 deriving (Show, Eq, Ord, Generic)

instance Serialise HiValue

data HiExpr = HiExprValue HiValue
                | HiExprApply HiExpr [HiExpr]
                | HiExprRun HiExpr
                | HiExprDict [(HiExpr, HiExpr)]
                deriving (Show, Eq, Ord)

data HiError = HiErrorInvalidArgument
                 | HiErrorInvalidFunction
                 | HiErrorArityMismatch
                 | HiErrorDivideByZero
                 deriving (Show, Eq)

data HiAction = HiActionRead  FilePath
                 | HiActionWrite FilePath ByteString
                 | HiActionMkDir FilePath
                 | HiActionChDir FilePath
                 | HiActionCwd
                 | HiActionNow
                 | HiActionRand Int Int
                 | HiActionEcho Text
                 deriving (Show, Eq, Ord, Generic)

instance Serialise HiAction

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
