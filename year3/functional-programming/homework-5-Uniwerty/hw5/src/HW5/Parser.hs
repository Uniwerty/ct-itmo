module HW5.Parser
  ( parse
  ) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word

import Control.Monad.Combinators.Expr
import HW5.Base
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void String

-- | Parse the given string into an expression with possible error
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (infixExpr <* eof) ""

-- | An infix expression parser
infixExpr :: Parser HiExpr
infixExpr = makeExprParser expr operatorTable

-- | An expression unit parser
expr :: Parser HiExpr
expr = do
  skipWS
  term <- choice [
    number,
    boolean,
    nullValue,
    stringLiteral,
    byteArrayLiteral,
    listLiteral,
    dictionary,
    cwd,
    now,
    function,
    parentheses
    ]
  invocations <- many $ choice [application, execution, dotAccess]
  return $ foldl (\x f -> f x) term invocations

number :: Parser HiExpr
number = lexeme $ HiExprValue . HiValueNumber . toRational <$> Lexer.signed skipWS Lexer.scientific

boolean :: Parser HiExpr
boolean = lexeme $ HiExprValue . HiValueBool <$> choice [
  False <$ string "false",
  True <$ string "true"
  ]

nullValue :: Parser HiExpr
nullValue = lexeme $ HiExprValue HiValueNull <$ string "null"

stringLiteral :: Parser HiExpr
stringLiteral = lexeme $ HiExprValue . HiValueString . Text.pack <$> do
  void (char '"')
  manyTill Lexer.charLiteral (char '"')

listLiteral :: Parser HiExpr
listLiteral = lexeme $ do
  elems <- between (lexeme $ char '[') (lexeme $ char ']') (sepBy infixExpr (lexeme $ char ','))
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) elems

byteArrayLiteral :: Parser HiExpr
byteArrayLiteral = lexeme $ do
  bytes <- between (lexeme $ string "[#") (lexeme $ string "#]") (sepBy byte skipWS)
  return $ HiExprValue $ HiValueBytes $ BS.pack bytes

byte :: Parser Word8
byte = lexeme $ do
  x1 <- digitValue <$> hexDigitChar
  x0 <- digitValue <$> hexDigitChar
  return $ fromInteger $ toInteger $ x1 * 16 + x0

digitValue :: Char -> Int
digitValue x
  | isDigit x = ord x - ord '0'
  | otherwise = ord (toLower x) - ord 'a' + 10

cwd :: Parser HiExpr
cwd = HiExprValue (HiValueAction HiActionCwd) <$ lexeme (string "cwd")

now :: Parser HiExpr
now = HiExprValue (HiValueAction HiActionNow) <$ lexeme (string "now")

dictionary :: Parser HiExpr
dictionary = do
  mappings <- between (lexeme $ char '{') (lexeme $ char '}') (sepBy mapping (lexeme $ char ','))
  return $ HiExprDict mappings

mapping :: Parser (HiExpr, HiExpr)
mapping = do
  key <- infixExpr
  void (lexeme $ char ':')
  value <- infixExpr
  return (key, value)

function :: Parser HiExpr
function = lexeme $ HiExprValue . HiValueFunction <$> do
    choice [
      HiFunAdd <$ string "add",
      HiFunSub <$ string "sub",
      HiFunMul <$ string "mul",
      HiFunDiv <$ string "div",
      HiFunLessThan <$ string "less-than",
      HiFunGreaterThan <$ string "greater-than",
      HiFunEquals <$ string "equals",
      HiFunNotLessThan <$ string "not-less-than",
      HiFunNotGreaterThan <$ string "not-greater-than",
      HiFunNotEquals <$ string "not-equals",
      HiFunNot <$ string "not",
      HiFunAnd <$ string "and",
      HiFunOr <$ string "or",
      HiFunIf <$ string "if",
      HiFunLength <$ string "length",
      HiFunToUpper <$ string "to-upper",
      HiFunToLower <$ string "to-lower",
      HiFunReverse <$ string "reverse",
      HiFunTrim <$ string "trim",
      HiFunList <$ string "list",
      HiFunRange <$ string "range",
      HiFunFold <$ string "fold",
      HiFunPackBytes <$ string "pack-bytes",
      HiFunUnpackBytes <$ string "unpack-bytes",
      HiFunZip <$ string "zip",
      HiFunUnzip <$ string "unzip",
      HiFunEncodeUtf8 <$ string "encode-utf8",
      HiFunDecodeUtf8 <$ string "decode-utf8",
      HiFunSerialise <$ string "serialise",
      HiFunDeserialise <$ string "deserialise",
      HiFunRead <$ string "read",
      HiFunWrite <$ string "write",
      HiFunMkDir <$ string "mkdir",
      HiFunChDir <$ string "cd",
      HiFunParseTime <$ string "parse-time",
      HiFunRand <$ string "rand",
      HiFunEcho <$ string "echo",
      HiFunCount <$ string "count",
      HiFunKeys <$ string "keys",
      HiFunValues <$ string "values",
      HiFunInvert <$ string "invert"
      ]

parentheses :: Parser HiExpr
parentheses = between (lexeme $ char '(') (lexeme $ char ')') infixExpr

application :: Parser (HiExpr -> HiExpr)
application = do
    args <- between (lexeme $ char '(') (lexeme $ char ')') (sepBy infixExpr (lexeme $ char ','))
    return $ \e -> HiExprApply e args

execution :: Parser (HiExpr -> HiExpr)
execution = do
  void (lexeme $ char '!')
  return HiExprRun

dotAccess :: Parser (HiExpr -> HiExpr)
dotAccess = do
  void $ char '.'
  identifierParts <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  return $ \e -> HiExprApply e [HiExprValue $ HiValueString $ Text.pack (List.intercalate "-" identifierParts)]

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = [
    [ InfixL $ determinedOperator "*" HiFunMul,
      InfixL $ ambiguousOperator "/" "=" HiFunDiv
    ],
    [ InfixL $ determinedOperator "+" HiFunAdd,
      InfixL $ determinedOperator "-" HiFunSub
    ],
    [ InfixN $ ambiguousOperator "<" "=" HiFunLessThan,
      InfixN $ ambiguousOperator ">" "=" HiFunGreaterThan,
      InfixN $ determinedOperator "<=" HiFunNotGreaterThan,
      InfixN $ determinedOperator ">=" HiFunNotLessThan,
      InfixN $ determinedOperator "==" HiFunEquals,
      InfixN $ determinedOperator "/=" HiFunNotEquals
    ],
    [ InfixR $ determinedOperator "&&" HiFunAnd ],
    [ InfixR $ determinedOperator "||" HiFunOr ]
  ]

-- | A parser of an operator which is the only possible
determinedOperator :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
determinedOperator sign fun = operator fun (string sign)

-- | A parser of an operator which can be a prefix of another operator
ambiguousOperator :: String -> String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
ambiguousOperator required forbidden fun = try $
  operator fun (string required <* notFollowedBy (string forbidden))

-- | A parser of a binary operator into a function application
operator :: HiFun -> Parser String -> Parser (HiExpr -> HiExpr -> HiExpr)
operator fun operatorParser =
  (\a b -> HiExprApply (HiExprValue (HiValueFunction fun)) [a, b]) <$ lexeme operatorParser

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme skipWS

skipWS :: Parser ()
skipWS = Lexer.space space1 empty empty
