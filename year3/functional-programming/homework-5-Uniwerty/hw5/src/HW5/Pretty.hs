module HW5.Pretty
  ( prettyValue
  ) where

import Codec.Binary.UTF8.String
import qualified Data.ByteString as BS
import Data.Char
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Ratio
import Data.Sequence
import Data.Word
import HW5.Base
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)

-- | Prettify the specified value to print
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber x)
  | denominator x == 1  = viaShow (numerator x)
  | isMultiple10 $ denominator x = viaShow (fromIntegral (numerator x) / fromIntegral (denominator x) :: Double)
  | quot (numerator x) (denominator x) == 0 =
    let n = denominator x
    in prettyFrac (rem (numerator x) n) n
  | otherwise = let
      m = numerator x
      n = denominator x
      int = quot m n
      frac = rem m n
      sign = if frac > 0 then "+" else "-"
      in viaShow int <+> pretty sign <+> prettyFrac (abs frac) n
prettyValue (HiValueBool True) = pretty "true"
prettyValue (HiValueBool False) = pretty "false"
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString text) = viaShow text
prettyValue (HiValueList s)
  | e :<| elems <- s =
    pretty "[" <>
    foldl (\a x -> a <> pretty ", " <> x)
    (prettyValue e)
    (prettyValue <$> toList elems) <>
    pretty "]"
  | otherwise = pretty "[]"
prettyValue (HiValueBytes bytes) =
  pretty "[#" <> foldl (<+>) emptyDoc (prettyByte <$> BS.unpack bytes) <+> pretty "#]"
prettyValue (HiValueTime time) =
  prettyValue (HiValueFunction HiFunParseTime) <>
  pretty "(" <>
  pretty "\"" <>
  viaShow time <>
  pretty "\"" <>
  pretty ")"
prettyValue (HiValueAction (HiActionRead path)) =
  prettyValue (HiValueFunction HiFunRead) <> pretty "(" <> viaShow path <> pretty ")"
prettyValue (HiValueAction (HiActionMkDir path)) =
  prettyValue (HiValueFunction HiFunMkDir) <> pretty "(" <> viaShow path <> pretty ")"
prettyValue (HiValueAction (HiActionChDir path)) =
  prettyValue (HiValueFunction HiFunChDir) <> pretty "(" <> viaShow path <> pretty ")"
prettyValue (HiValueAction (HiActionWrite path bytes)) =
  prettyValue (HiValueFunction HiFunWrite) <>
  pretty "(" <>
  viaShow path <>
  pretty ", " <>
  viaShow (decode $ BS.unpack bytes) <>
  pretty ")"
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueAction (HiActionRand a b)) =
  prettyValue (HiValueFunction HiFunRand) <>
  pretty "(" <>
  pretty a <>
  pretty ", "
  <> pretty b
  <> pretty ")"
prettyValue (HiValueAction (HiActionEcho text)) =
  prettyValue (HiValueFunction HiFunEcho) <>
  pretty "(" <>
  viaShow text <>
  pretty ")"
prettyValue (HiValueDict m)
  | not $ Map.null m =
    let pairs = Map.assocs m in
    pretty "{" <+>
    foldl (\x mapping -> x <> pretty "," <+> prettyMapping mapping)
     (prettyMapping $ head pairs)
     (tail pairs) <+>
    pretty "}"
  | otherwise = pretty "{}"
prettyValue (HiValueFunction HiFunAdd) = pretty "add"
prettyValue (HiValueFunction HiFunSub) = pretty "sub"
prettyValue (HiValueFunction HiFunMul) = pretty "mul"
prettyValue (HiValueFunction HiFunDiv) = pretty "div"
prettyValue (HiValueFunction HiFunNot) = pretty "not"
prettyValue (HiValueFunction HiFunOr) = pretty "or"
prettyValue (HiValueFunction HiFunAnd) = pretty "and"
prettyValue (HiValueFunction HiFunLessThan) = pretty "less-than"
prettyValue (HiValueFunction HiFunGreaterThan) = pretty "greater-than"
prettyValue (HiValueFunction HiFunEquals) = pretty "equals"
prettyValue (HiValueFunction HiFunNotLessThan) = pretty "not-less-than"
prettyValue (HiValueFunction HiFunNotGreaterThan) = pretty "not-greater-than"
prettyValue (HiValueFunction HiFunNotEquals) = pretty "not-equals"
prettyValue (HiValueFunction HiFunIf) = pretty "if"
prettyValue (HiValueFunction HiFunLength) = pretty "length"
prettyValue (HiValueFunction HiFunToUpper) = pretty "to-upper"
prettyValue (HiValueFunction HiFunToLower) = pretty "to-lower"
prettyValue (HiValueFunction HiFunReverse) = pretty "reverse"
prettyValue (HiValueFunction HiFunTrim) = pretty "trim"
prettyValue (HiValueFunction HiFunList) = pretty "list"
prettyValue (HiValueFunction HiFunRange) = pretty "range"
prettyValue (HiValueFunction HiFunFold) = pretty "fold"
prettyValue (HiValueFunction HiFunPackBytes) = pretty "pack-bytes"
prettyValue (HiValueFunction HiFunUnpackBytes) = pretty "unpack-bytes"
prettyValue (HiValueFunction HiFunEncodeUtf8) = pretty "encode-utf8"
prettyValue (HiValueFunction HiFunDecodeUtf8) = pretty "decode-utf8"
prettyValue (HiValueFunction HiFunZip) = pretty "zip"
prettyValue (HiValueFunction HiFunUnzip) = pretty "unzip"
prettyValue (HiValueFunction HiFunSerialise) = pretty "serialise"
prettyValue (HiValueFunction HiFunDeserialise) = pretty "deserialise"
prettyValue (HiValueFunction HiFunRead) = pretty "read"
prettyValue (HiValueFunction HiFunWrite) = pretty "write"
prettyValue (HiValueFunction HiFunMkDir) = pretty "mkdir"
prettyValue (HiValueFunction HiFunChDir) = pretty "cd"
prettyValue (HiValueFunction HiFunParseTime) = pretty "parse-time"
prettyValue (HiValueFunction HiFunRand) = pretty "rand"
prettyValue (HiValueFunction HiFunEcho) = pretty "echo"
prettyValue (HiValueFunction HiFunCount) = pretty "count"
prettyValue (HiValueFunction HiFunKeys) = pretty "keys"
prettyValue (HiValueFunction HiFunValues) = pretty "values"
prettyValue (HiValueFunction HiFunInvert) = pretty "invert"

-- | Prettify a fraction of two integers
prettyFrac :: Integer -> Integer -> Doc AnsiStyle
prettyFrac m n = viaShow m <> pretty "/" <> viaShow n

isMultiple10 :: Integer -> Bool
isMultiple10 0 = True
isMultiple10 1 = True
isMultiple10 n
  | even n = isMultiple10 (quot n 2)
  | rem n 5 == 0 = isMultiple10 (quot n 5)
  | otherwise = False

-- | Prettify a byte into hexadecimal form
prettyByte :: Word8 -> Doc AnsiStyle
prettyByte byte = prettyHexDigit (quot byte 16) <> prettyHexDigit (rem byte 16)

-- | Prettify a digit into hexadecimal form
prettyHexDigit :: Word8 -> Doc AnsiStyle
prettyHexDigit x
  | x < 10 = pretty $ chr $ ord '0' + fromIntegral x
  | otherwise = pretty $ chr $ ord 'a' + fromIntegral x - 10

-- | Prettify a dictionary mapping
prettyMapping :: (HiValue, HiValue) -> Doc AnsiStyle
prettyMapping (key, value) = prettyValue key <> pretty ":" <+> prettyValue value
