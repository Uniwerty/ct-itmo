module Main (
main
) where

import HW4.T1
import HW4.T2
import HW4.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
--  HW4.T1 tests
  describe "HW4.T1" $ do
    it "mapExceptState" $ do
      let mappedState = mapExceptState (* 5) (ES $ \s -> Success $ 2 :# s)
      runES mappedState "mapped"
      `shouldBe` (Success ((2 * 5) :# "mapped") :: Except EvaluationError (Annotated String Int))

    it "wrapExceptState" $ do
      let wrappedState = wrapExceptState 2
      runES wrappedState "wrapped"
      `shouldBe` (Success (2 :# "wrapped") :: Except EvaluationError (Annotated String Int))

    it "joinExceptState" $ do
      let jointState = joinExceptState $ ES $ \s -> Success (ES (\s2 -> Success (2 :# s2)) :# s)
      runES jointState "joint"
      `shouldBe` (Success (2 :# "joint") :: Except EvaluationError (Annotated String Int))

    it "modifyExceptState" $ do
      let modifiedState = modifyExceptState ("now: "++)
      runES modifiedState "modified"
      `shouldBe` (Success (() :# "now: modified") :: Except EvaluationError (Annotated String ()))

    it "throwExceptState" $ do
      let thrownState = throwExceptState DivideByZero
      runES thrownState "thrown"
      `shouldBe` (Error DivideByZero :: Except EvaluationError (Annotated String Int))

    it "eval success" $ do
      runES (eval (2 + 5 * 3 + 9 / signum (-100))) []
      `shouldBe` (Success (8.0 :#
          [
            Add 17.0 (-9.0),
            Div 9.0 (-1.0),
            Sgn (-100.0),
            Sub 0.0 100.0,
            Add 2.0 15.0,
            Mul 5.0 3.0
          ]
        ) :: Except EvaluationError (Annotated [Prim Double] Double))

    it "eval error" $ do
      runES (eval (2 * 5 + 1 / 0)) []
      `shouldBe` (Error DivideByZero :: Except EvaluationError (Annotated [Prim Double] Double))

--  HW4.T2 tests

  describe "HW4.T2" $ do
    it "pChar empty" $ do
      runP pChar "" `shouldBe` (Error $ ErrorAtPos 0 :: Except ParseError Char)

    it "pChar success" $ do
      runP pChar "abc" `shouldBe` (Success 'a' :: Except ParseError Char)

    it "parseError" $ do
      runP parseError "abc" `shouldBe` (Error $ ErrorAtPos 0 :: Except ParseError Char)

    it "pEof empty" $ do
      runP pEof "" `shouldBe` (Success () :: Except ParseError ())

    it "pEof error" $ do
      runP pEof "abc" `shouldBe` (Error $ ErrorAtPos 0 :: Except ParseError ())

    it "parseExpr integer" $ do
      parseExpr "100" `shouldBe` (Success $ Val 100 :: Except ParseError Expr)

    it "parseExpr fraction" $ do
      parseExpr "0.001" `shouldBe` (Success $ Val 0.001 :: Except ParseError Expr)

    it "parseExpr negative" $ do
      parseExpr "-100.99" `shouldBe` (Success $ Val (-100.99) :: Except ParseError Expr)

    it "parseExpr positive" $ do
      parseExpr "+100.99" `shouldBe` (Success $ Val 100.99 :: Except ParseError Expr)

    it "parseExpr parentheses" $ do
      parseExpr "(((100)))" `shouldBe` (Success $ Val 100 :: Except ParseError Expr)

    it "parseExpr whitespace" $ do
      parseExpr "  \n\n  \t  100  \r  " `shouldBe` (Success $ Val 100 :: Except ParseError Expr)

    it "parseExpr add" $ do
      parseExpr "2 + 9" `shouldBe` (Success $ Op $ Add (Val 2.0) (Val 9.0) :: Except ParseError Expr)

    it "parseExpr sub" $ do
      parseExpr "2 - 9" `shouldBe` (Success $ Op $ Sub (Val 2.0) (Val 9.0) :: Except ParseError Expr)

    it "parseExpr mul" $ do
      parseExpr "2 * 9" `shouldBe` (Success $ Op $ Mul (Val 2.0) (Val 9.0) :: Except ParseError Expr)

    it "parseExpr div" $ do
      parseExpr "2 / 9" `shouldBe` (Success $ Op $ Div (Val 2.0) (Val 9.0) :: Except ParseError Expr)

    it "parseExpr chain" $ do
      parseExpr "1 + 2 + 3 + 4"
      `shouldBe` (Success $
        Op $ Add
          (Op $ Add
            (Op $ Add
              (Val 1.0)
              (Val 2.0)
            )
            (Val 3.0)
          )
          (Val 4.0)
        :: Except ParseError Expr)

    it "parseExpr mixed" $ do
      parseExpr "1 + 2 * 3 - 4 / 5"
      `shouldBe` (Success $
        Op $ Sub
          (Op $ Add
            (Val 1.0)
            (Op $ Mul
              (Val 2.0)
              (Val 3.0)
            )
          )
          (Op $ Div
            (Val 4.0)
            (Val 5.0)
          )
        :: Except ParseError Expr)

    it "parseExpr error" $ do
      parseExpr "1 + ..." `shouldBe` (Error $ ErrorAtPos 2 :: Except ParseError Expr)
