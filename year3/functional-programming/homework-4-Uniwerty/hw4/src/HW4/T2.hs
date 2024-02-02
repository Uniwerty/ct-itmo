{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{-|
Module: HW4.T2
Description: A module with Parser type, its instances,
some primitive parsers and expression parser.
-}
module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , pEof
  , parseExpr
  ) where

import Control.Applicative
import Control.Monad
import Data.Char (digitToInt, isDigit, isSpace)
import Numeric.Natural (Natural)

import HW4.T1 (ExceptState (..))
import HW4.Types

newtype ParseError = ErrorAtPos Natural
  deriving (Show, Eq)

-- | A parser from 'String' to some type, handling 'ParseError'
newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Parses the given string with the given parser
runP :: Parser a -> String -> Except ParseError a
runP (P state) string =
  case runES state (0, string) of
    Error e          -> Error e
    Success (a :# _) -> Success a

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
-- | Single character parser
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

-- | Constantly failing parser
parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (P state1) <|> (P state2) = P $ ES $ \s ->
    let result1 = runES state1 s
    in case result1 of
      Error _ -> runES state2 s
      _       -> result1

-- No methods
instance MonadPlus Parser

-- | End of string parser
pEof :: Parser ()
pEof = P $ ES $ \state@(pos, s) ->
  case s of
    [] -> Success $ () :# state
    _  -> Error $ ErrorAtPos pos

-- | Arithmetical expression parser
parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ do
  expr <- pExpr
  pEof
  return expr

pExpr :: Parser Expr
pExpr = do
  first <- pSummand
  optionalAddSub first

pSummand :: Parser Expr
pSummand = do
  first <- pMultiplier
  optionalMulDiv first

pMultiplier :: Parser Expr
pMultiplier = pDouble <|> pParentheses

optionalAddSub :: Expr -> Parser Expr
optionalAddSub first = continueAdd first <|> continueSub first <|> return first

optionalMulDiv :: Expr -> Parser Expr
optionalMulDiv first = continueMul first <|> continueDiv first <|> return first

continueAdd :: Expr -> Parser Expr
continueAdd = continueOperation pSummand optionalAddSub Add '+'

continueSub :: Expr -> Parser Expr
continueSub = continueOperation pSummand optionalAddSub Sub '-'

continueMul :: Expr -> Parser Expr
continueMul = continueOperation pMultiplier optionalMulDiv Mul '*'

continueDiv :: Expr -> Parser Expr
continueDiv = continueOperation pMultiplier optionalMulDiv Div '/'

continueOperation :: Parser Expr
                     -> (Expr -> Parser Expr)
                     -> (Expr -> Expr -> Prim Expr)
                     -> Char
                     -> Expr
                     -> Parser Expr
continueOperation secondParser optionalParser operation sign first = do
  skipParticularChar sign
  second <- secondParser
  optionalParser $ Op $ operation first second

pParentheses :: Parser Expr
pParentheses = do
  skipWhitespace
  skipParticularChar '('
  skipWhitespace
  expr <- pExpr
  skipWhitespace
  skipParticularChar ')'
  skipWhitespace
  return expr

pDouble :: Parser Expr
pDouble = do
  skipWhitespace
  sign <- pNumberSign
  integer <- pInteger
  fraction <- pFraction
  skipWhitespace
  return $ Val $ sign * (integer + fraction)

pNumberSign :: Parser Double
pNumberSign = do
  sign <- optional $ pSatisfiedChar $ \c -> c == '-' || c == '+'
  return $ case sign of
    Just '-' -> -1
    _        -> 1

pInteger :: Parser Double
pInteger = foldl (\prevD d -> fromIntegral (digitToInt d) + prevD * 10) 0 <$> pDigits

pFraction :: Parser Double
pFraction = do
  fraction <- optional (do skipParticularChar '.'; pDigits)
  return $ case fraction of
    Just x -> foldr (\d prevD -> fromIntegral (digitToInt d) + prevD / 10) 0 x / 10
    _      -> 0

skipWhitespace :: Parser ()
skipWhitespace = void $ many $ pSatisfiedChar isSpace

skipParticularChar :: Char -> Parser ()
skipParticularChar char = void $ pSatisfiedChar (== char)

pDigits :: Parser String
pDigits = some $ pSatisfiedChar isDigit

pSatisfiedChar :: (Char -> Bool) -> Parser Char
pSatisfiedChar condition = mfilter condition pChar
