module HW5.Evaluator
  ( eval
  ) where

import Codec.Binary.UTF8.String
import Codec.Compression.Zlib
import qualified Codec.Serialise as Serialise
import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Ratio
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Word
import HW5.Base
import Text.Read

-- | Evaluate an expression with possible error
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval e = runExceptT (evalExcept e)

evalExcept :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExcept (HiExprValue value) = return value
evalExcept (HiExprApply expr args) = do
  fun <- evalExcept expr
  case fun of
    HiValueFunction f -> evalFunction f args
    HiValueString s   -> evalStringSubset s args
    HiValueList s     -> evalListSubset s args
    HiValueBytes b    -> evalBytesSubset b args
    HiValueDict m     -> evalMap m args
    _                 -> throwE HiErrorInvalidFunction
evalExcept (HiExprRun expr) = do
  action <- evalExcept expr
  case action of
    HiValueAction a -> ExceptT $ Right <$> runAction a
    _               -> throwE HiErrorInvalidArgument
evalExcept (HiExprDict pairs) = do
  mappings <- mapM (
    \(first, second) -> do
    a <- evalExcept first
    b <- evalExcept second
    return (a, b)
    ) pairs
  return $ HiValueDict $ Map.fromList mappings

-- | Evaluate the given function
evalFunction :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalFunction HiFunList elems = do
  s <- mapM evalExcept (Seq.fromList elems)
  return $ HiValueList s
evalFunction f [a] = do
  arg <- evalExcept a
  case f of
      HiFunNot         -> evalNot arg
      HiFunLength      -> evalLength arg
      HiFunToUpper     -> evalStrOp Text.toUpper arg
      HiFunToLower     -> evalStrOp Text.toLower arg
      HiFunReverse     -> evalReverse arg
      HiFunTrim        -> evalStrOp Text.strip arg
      HiFunPackBytes   -> evalPackBytes arg
      HiFunUnpackBytes -> evalUnpackBytes arg
      HiFunEncodeUtf8  -> evalEncodeUtf8 arg
      HiFunDecodeUtf8  -> evalDecodeUtf8 arg
      HiFunZip         -> evalZip arg
      HiFunUnzip       -> evalUnzip arg
      HiFunSerialise   -> evalSerialise arg
      HiFunDeserialise -> evalDeserialise arg
      HiFunRead        -> evalPathAction HiActionRead arg
      HiFunMkDir       -> evalPathAction HiActionMkDir arg
      HiFunChDir       -> evalPathAction HiActionChDir arg
      HiFunParseTime   -> evalParseTime arg
      HiFunEcho        -> evalEcho arg
      HiFunCount       -> evalCount arg
      HiFunKeys        -> evalKeys arg
      HiFunValues      -> evalValues arg
      HiFunInvert      -> evalInvert arg
      _                -> throwE HiErrorArityMismatch
evalFunction HiFunOr [a1, a2] = do
  first <- evalExcept a1
  case first of
    HiValueNull -> evalExcept a2
    HiValueBool value ->
      if value
      then return first
      else evalExcept a2
    _ -> return first
evalFunction HiFunAnd [a1, a2] = do
  first <- evalExcept a1
  case first of
    HiValueNull -> return first
    HiValueBool value ->
      if value
      then evalExcept a2
      else return first
    _ -> evalExcept a2
evalFunction f [a1, a2] = do
  first <- evalExcept a1
  second <- evalExcept a2
  case f of
      HiFunAdd -> evalAddOp first second
      HiFunSub -> evalSubOp first second
      HiFunMul -> evalMulOp first second
      HiFunDiv -> case second of
        HiValueNumber 0 -> throwE HiErrorDivideByZero
        _               -> evalDivOp first second
      HiFunLessThan -> evalComparison LT first second
      HiFunGreaterThan -> evalComparison GT first second
      HiFunEquals -> evalComparison EQ first second
      HiFunNotLessThan -> evalComparison LT first second >>= evalNot
      HiFunNotGreaterThan -> evalComparison GT first second >>= evalNot
      HiFunNotEquals -> evalComparison EQ first second >>= evalNot
      HiFunRange -> evalRange first second
      HiFunFold -> evalFold first second
      HiFunWrite -> evalWriteAction first second
      HiFunRand -> evalRand first second
      _ -> throwE HiErrorArityMismatch
evalFunction HiFunIf [a1, a2, a3] = evalIf a1 a2 a3
evalFunction _ _ = throwE HiErrorArityMismatch

evalBinaryNumberOp :: HiMonad m => (Rational -> Rational -> Rational) ->
                                    HiValue -> HiValue ->
                                    ExceptT HiError m HiValue
evalBinaryNumberOp op (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber $ op a b
evalBinaryNumberOp _ _ _                                  = throwE HiErrorInvalidArgument

evalNot :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalNot (HiValueBool a) = return $ HiValueBool $ not a
evalNot _               = throwE HiErrorInvalidArgument

evalComparison :: HiMonad m => Ordering -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalComparison ord (HiValueBool _) (HiValueNumber _) = return $ HiValueBool $ compare False True == ord
evalComparison ord (HiValueNumber _) (HiValueBool _)  = return $ HiValueBool $ compare True False == ord
evalComparison ord a b = return $ HiValueBool $ compare a b == ord

evalIf :: HiMonad m => HiExpr -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalIf predicate first second = do
  condition <- evalExcept predicate
  case condition of
     HiValueBool True  -> evalExcept first
     HiValueBool False -> evalExcept second
     _                 -> throwE HiErrorInvalidArgument

evalLength :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalLength (HiValueString str) = return $ HiValueNumber $ fromIntegral $ Text.length str
evalLength (HiValueList s)     = return $ HiValueNumber $ fromIntegral $ Seq.length s
evalLength (HiValueBytes b)    = return $ HiValueNumber $ fromIntegral $ BS.length b
evalLength _                   = throwE HiErrorInvalidArgument

evalReverse :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalReverse (HiValueList s)     = return $ HiValueList $ Seq.reverse s
evalReverse s@(HiValueString _) = evalStrOp Text.reverse s
evalReverse (HiValueBytes b)    = return $ HiValueBytes $ BS.reverse b
evalReverse _                   = throwE HiErrorInvalidArgument

evalStrOp :: HiMonad m => (Text.Text -> Text.Text) -> HiValue -> ExceptT HiError m HiValue
evalStrOp op (HiValueString str) = return $ HiValueString $ op str
evalStrOp _ _                    = throwE HiErrorInvalidArgument

evalAddOp :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalAddOp (HiValueString s) (HiValueString t) = return $ HiValueString $ Text.append s t
evalAddOp (HiValueList s) (HiValueList t)     = return $ HiValueList $ s Seq.>< t
evalAddOp (HiValueBytes a) (HiValueBytes b)   = return $ HiValueBytes $ BS.append a b
evalAddOp (HiValueTime t) (HiValueNumber n)   = return $ HiValueTime $ addUTCTime (fromRational n) t
evalAddOp a b                                 = evalBinaryNumberOp (+) a b

evalSubOp :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalSubOp (HiValueTime a) (HiValueTime b) = return $ HiValueNumber $ toRational $ diffUTCTime a b
evalSubOp a b                             = evalBinaryNumberOp (-) a b

evalMulOp :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalMulOp (HiValueString s) (HiValueNumber n) = HiValueString <$> replicateOp n s
evalMulOp (HiValueList s) (HiValueNumber n)   = HiValueList <$> replicateOp n s
evalMulOp (HiValueBytes b) (HiValueNumber n)  = HiValueBytes <$> replicateOp n b
evalMulOp a b                                 = evalBinaryNumberOp (*) a b

replicateOp :: (Semigroup a, HiMonad m) => Rational -> a -> ExceptT HiError m a
replicateOp n a
  | denominator n == 1 = return $ stimes (intNumerator n) a
  | otherwise = throwE HiErrorInvalidArgument

evalDivOp :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalDivOp (HiValueString s) (HiValueString t) = return $ HiValueString $ Text.concat [s, Text.pack "/", t]
evalDivOp a b = evalBinaryNumberOp (/) a b

intNumerator :: Rational -> Int
intNumerator n = fromIntegral $ numerator n :: Int

evalStringSubset :: HiMonad m => Text.Text -> [HiExpr] -> ExceptT HiError m HiValue
evalStringSubset = evalMainSlice evalStringSlice lookupFun
  where lookupFun :: Text.Text -> Int -> HiValue
        lookupFun t i = if i >= 0 && i < Text.length t
                        then HiValueString $ Text.singleton $ Text.index t i
                        else HiValueNull

evalStringSlice :: HiMonad m => Text.Text -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalStringSlice = evalSlice evalNormalStringSlice Text.length HiValueString

evalNormalStringSlice :: HiMonad m => Text.Text -> Int -> Int -> ExceptT HiError m HiValue
evalNormalStringSlice = evalIntSlice Text.empty Text.length Text.drop Text.take HiValueString

evalListSubset :: HiMonad m => Seq.Seq HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalListSubset = evalMainSlice evalListSlice lookupFun
  where lookupFun :: Seq.Seq HiValue -> Int -> HiValue
        lookupFun s i = let el = Seq.lookup i s
                        in case el of
                          Just val -> val
                          _        -> HiValueNull

evalListSlice :: HiMonad m => Seq.Seq HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalListSlice = evalSlice evalNormalListSlice Seq.length HiValueList

evalNormalListSlice :: HiMonad m => Seq.Seq HiValue -> Int -> Int -> ExceptT HiError m HiValue
evalNormalListSlice = evalIntSlice Seq.Empty Seq.length Seq.drop Seq.take HiValueList

evalBytesSubset :: HiMonad m => BS.ByteString -> [HiExpr] -> ExceptT HiError m HiValue
evalBytesSubset = evalMainSlice evalBytesSlice lookupFun
  where lookupFun :: BS.ByteString -> Int -> HiValue
        lookupFun b i = if i >= 0 && i < BS.length b
                        then HiValueNumber $ toRational $ BS.index b i
                        else HiValueNull


evalBytesSlice :: HiMonad m => BS.ByteString -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalBytesSlice = evalSlice evalNormalBytesSlice BS.length HiValueBytes

evalNormalBytesSlice :: HiMonad m => BS.ByteString -> Int -> Int -> ExceptT HiError m HiValue
evalNormalBytesSlice = evalIntSlice BS.empty BS.length BS.drop BS.take HiValueBytes

evalMainSlice :: HiMonad m => (a -> HiValue -> HiValue -> ExceptT HiError m HiValue) ->
                              (a -> Int -> HiValue) ->
                              a ->
                              [HiExpr] ->
                              ExceptT HiError m HiValue
evalMainSlice _ lookupFun x [a] = do
  value <- evalExcept a
  case value of
    HiValueNumber n -> let i = intNumerator n in
      if denominator n == 1
      then return $ lookupFun x i
      else throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidArgument
evalMainSlice sliceFun _ x [a1, a2] = do
  l <- evalExcept a1
  r <- evalExcept a2
  sliceFun x l r
evalMainSlice _ _ _ _ = throwE HiErrorArityMismatch

evalSlice :: HiMonad m => (a -> Int -> Int -> ExceptT HiError m HiValue) ->
                          (a -> Int) ->
                          (a -> HiValue) ->
                          a ->
                          HiValue ->
                          HiValue ->
                          ExceptT HiError m HiValue
evalSlice _ _ valueFun a HiValueNull HiValueNull = return $ valueFun a
evalSlice intSliceFun _ _ a HiValueNull (HiValueNumber r)
 | denominator r == 1 = intSliceFun a 0 (intNumerator r)
 | otherwise = throwE HiErrorInvalidArgument
evalSlice intSliceFun lengthFun _ a (HiValueNumber l) HiValueNull
 | denominator l == 1 = intSliceFun a (intNumerator l) (lengthFun a)
 | otherwise = throwE HiErrorInvalidArgument
evalSlice intSliceFun _ _ a (HiValueNumber l) (HiValueNumber r)
 | denominator l == 1 && denominator r == 1 = intSliceFun a (intNumerator l) (intNumerator r)
 | otherwise = throwE HiErrorInvalidArgument
evalSlice _ _ _ _ _ _ = throwE HiErrorInvalidArgument

evalIntSlice :: HiMonad m => a ->
                                (a -> Int) ->
                                (Int -> a -> a) ->
                                (Int -> a -> a) ->
                                (a -> HiValue) ->
                                a ->
                                Int ->
                                Int ->
                                ExceptT HiError m HiValue
evalIntSlice emptyValue lengthFun dropFun takeFun valueFun a left right
  | (l < 0 || l > len) && (r < 0 || r > len) || l == r = return $ valueFun emptyValue
  | l >= 0 && l < len && (r <= 0 || r > len) = return $ valueFun $ dropFun l a
  | (l < 0 || l >= len) && r > 0 && r <= len = return $ valueFun $ takeFun r a
  | otherwise = return $ valueFun $ dropFun l $ takeFun r a
  where len = lengthFun a
        l = correctIndex left len
        r = correctIndex right len

-- | Get the index in terms of slices
correctIndex :: Int -> Int -> Int
correctIndex i len
  | i < 0 = len + i
  | otherwise = i

evalRange :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalRange (HiValueNumber a) (HiValueNumber b)
  | b <= a = return $ HiValueList Seq.Empty
  | otherwise = return $ HiValueList $ Seq.fromFunction (floor $ b - a + 1) (\i -> HiValueNumber $ a + fromIntegral i)
evalRange _ _ = throwE HiErrorInvalidArgument

evalFold :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalFold (HiValueFunction _) list@(HiValueList Seq.Empty) = return list
evalFold fun@(HiValueFunction _) (HiValueList (a Seq.:<| s)) =
  foldM (\x y -> evalExcept $ HiExprApply (HiExprValue fun) [HiExprValue x, HiExprValue y]) a s
evalFold _ _ = throwE HiErrorInvalidArgument

evalPackBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalPackBytes (HiValueList s) = do
  bytes <- mapM packByte (toList s)
  return $ HiValueBytes $ BS.pack bytes
evalPackBytes _ = throwE HiErrorInvalidArgument

packByte :: HiMonad m => HiValue -> ExceptT HiError m Word8
packByte (HiValueNumber x)
  | denominator x == 1 && n >= 0 && n <= 255 = return $ fromIntegral n
  where n = intNumerator x
packByte _ = throwE HiErrorInvalidArgument

evalUnpackBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalUnpackBytes (HiValueBytes bytes) =
  return $ HiValueList $ Seq.fromList $ map (HiValueNumber . toRational) (BS.unpack bytes)
evalUnpackBytes _ = throwE HiErrorInvalidArgument

evalEncodeUtf8 :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalEncodeUtf8 (HiValueString text)
  | isUTF8Encoded str = return $ HiValueBytes $ BS.pack $ encode str
  | otherwise = throwE HiErrorInvalidArgument
  where str = Text.unpack text
evalEncodeUtf8 _ = throwE HiErrorInvalidArgument

evalDecodeUtf8 :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalDecodeUtf8 (HiValueBytes bytes)
  | isUTF8Encoded str = return $ HiValueString $ Text.pack str
  | otherwise = return HiValueNull
  where str = decode $ BS.unpack bytes
evalDecodeUtf8 _ = throwE HiErrorInvalidArgument

evalZip :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalZip (HiValueBytes bytes) =
  return $ HiValueBytes $ LBS.toStrict $
    compressWith defaultCompressParams { compressLevel = bestCompression } (LBS.fromStrict bytes :: LBS.ByteString)
evalZip _ = throwE HiErrorInvalidArgument

evalUnzip :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalUnzip (HiValueBytes bytes) =
  return $ HiValueBytes $ LBS.toStrict $
    decompressWith defaultDecompressParams (LBS.fromStrict bytes :: LBS.ByteString)
evalUnzip _ = throwE HiErrorInvalidArgument

evalSerialise :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalSerialise value = return $ HiValueBytes $ LBS.toStrict $ Serialise.serialise value

evalDeserialise :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalDeserialise (HiValueBytes bytes) =
  let result = Serialise.deserialiseOrFail $ LBS.fromStrict bytes
  in case result of
    Right value -> return value
    _           -> throwE HiErrorInvalidArgument
evalDeserialise _ = throwE HiErrorInvalidArgument

-- | Evaluate an action which argument is a file path
evalPathAction :: HiMonad m => (FilePath -> HiAction) -> HiValue -> ExceptT HiError m HiValue
evalPathAction action (HiValueString str) = return $ HiValueAction $ action $ Text.unpack str
evalPathAction _ _                        = throwE HiErrorInvalidArgument

evalWriteAction :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalWriteAction (HiValueString path) (HiValueString str) =
  return $ HiValueAction $ HiActionWrite (Text.unpack path) (BS.pack $ encode $ Text.unpack str)
evalWriteAction _ _ = throwE HiErrorInvalidArgument

evalParseTime :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalParseTime (HiValueString str) = return $
  case readMaybe (Text.unpack str) of
    Just time -> HiValueTime time
    _         -> HiValueNull
evalParseTime _ = throwE HiErrorInvalidArgument

evalRand :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalRand (HiValueNumber a) (HiValueNumber b)
  | denominator a == 1 && denominator b == 1 =
    return $ HiValueAction $ HiActionRand (intNumerator a) (intNumerator b)
evalRand _ _ = throwE HiErrorInvalidArgument

evalEcho :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalEcho (HiValueString text) = return $ HiValueAction $ HiActionEcho text
evalEcho _                    = throwE HiErrorInvalidArgument

evalMap :: HiMonad m => Map.Map HiValue HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalMap m [a] = do
  key <- evalExcept a
  return $ Map.findWithDefault HiValueNull key m
evalMap _ _ = throwE HiErrorInvalidArgument

evalCount :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalCount (HiValueString t) = return $ HiValueDict $
  Map.map (HiValueNumber . fromInteger) $
  foldr (\c m -> Map.insertWith (+) (HiValueString $ Text.pack [c]) 1 m) Map.empty (Text.unpack t)
evalCount (HiValueList s) = return $ HiValueDict $
  Map.map (HiValueNumber . fromInteger) $
  foldr (\c m -> Map.insertWith (+) c 1 m) Map.empty (toList s)
evalCount (HiValueBytes b) = return $ HiValueDict $
  Map.map (HiValueNumber . fromInteger) $
  foldr (\c m -> Map.insertWith (+) (HiValueNumber $ toRational c) 1 m) Map.empty (BS.unpack b)
evalCount _ = throwE HiErrorInvalidArgument

evalKeys :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalKeys (HiValueDict m) = return $ HiValueList $ Seq.fromList $ Map.keys m
evalKeys _               = throwE HiErrorInvalidArgument

evalValues :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalValues (HiValueDict m) = return $ HiValueList $ Seq.fromList $ Map.elems m
evalValues _               = throwE HiErrorInvalidArgument

evalInvert :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalInvert (HiValueDict dict) = return $ HiValueDict $
                               Map.map HiValueList $
                               foldl (\m (key, value) ->
                                Map.insertWith (Seq.><) value (Seq.singleton key) m) Map.empty (Map.assocs dict)
evalInvert _ = throwE HiErrorInvalidArgument
