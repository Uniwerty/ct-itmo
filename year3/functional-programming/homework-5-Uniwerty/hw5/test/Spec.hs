import HW5.Parser
import HW5.Base
import Test.Hspec
import Data.Ratio
import qualified Data.Text as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "HW5.Parser" $ do
    it "integral number" $ do
      parse "12" `shouldBe` (Right $ HiExprValue $ HiValueNumber $ 12 % 1)
      parse "+12" `shouldBe` (Right $ HiExprValue $ HiValueNumber $ 12 % 1)
      parse "-12" `shouldBe` (Right $ HiExprValue $ HiValueNumber $ -12 % 1)
      
    it "float number" $ do
      parse "12.005e2" `shouldBe` (Right $ HiExprValue $ HiValueNumber $ 2401 % 2)
      parse "12.005E2" `shouldBe` (Right $ HiExprValue $ HiValueNumber $ 2401 % 2)
      parse "12.005E-2" `shouldBe` (Right $ HiExprValue $ HiValueNumber $ 2401 % 20000)
      parse "-12.005e2" `shouldBe` (Right $ HiExprValue $ HiValueNumber $ -2401 % 2)
      parse "+12.005e2" `shouldBe` (Right $ HiExprValue $ HiValueNumber $ 2401 % 2)
      
    it "function application" $ do
      parse "add(1, 1)" `shouldBe` 
        (Right $ HiExprApply (HiExprValue $ HiValueFunction HiFunAdd) 
        [HiExprValue $ HiValueNumber $ 1 % 1, HiExprValue $ HiValueNumber $ 1 % 1])
      parse "sub(1, 1)" `shouldBe` 
        (Right $ HiExprApply (HiExprValue $ HiValueFunction HiFunSub) 
        [HiExprValue $ HiValueNumber $ 1 % 1, HiExprValue $ HiValueNumber $ 1 % 1])
      parse "mul(2, 2)" `shouldBe` 
        (Right $ HiExprApply (HiExprValue $ HiValueFunction HiFunMul) 
        [HiExprValue $ HiValueNumber $ 2 % 1, HiExprValue $ HiValueNumber $ 2 % 1])
      parse "div(2, 2)" `shouldBe` 
        (Right $ HiExprApply (HiExprValue $ HiValueFunction HiFunDiv) 
        [HiExprValue $ HiValueNumber $ 2 % 1, HiExprValue $ HiValueNumber $ 2 % 1])
      parse "length(\"Hello\")" `shouldBe` 
        (Right $ HiExprApply (HiExprValue $ HiValueFunction HiFunLength) 
        [HiExprValue $ HiValueString $ Text.pack "Hello"])
        
    it "action execution" $ do
      parse "read(\"unknown.txt\")!" `shouldBe` 
        (Right $ HiExprRun $ HiExprApply (HiExprValue $ HiValueFunction HiFunRead) 
        [HiExprValue $ HiValueString $ Text.pack "unknown.txt"])
        
      parse "now!" `shouldBe` 
        (Right $ HiExprRun $ HiExprValue $ HiValueAction HiActionNow)
        
    it "dictionary" $ do
      parse "{}" `shouldBe` (Right $ HiExprDict [])
      
      parse "{ true: 1, 2: [] }" `shouldBe` 
        (Right $ HiExprDict 
        [(HiExprValue $ HiValueBool True, HiExprValue $ HiValueNumber $ 1 % 1), 
        (HiExprValue $ HiValueNumber $ 2 % 1, HiExprApply (HiExprValue $ HiValueFunction HiFunList) [])])
      
      parse "{ \"a\": 1 }.a" `shouldBe`
        (Right $ HiExprApply 
        (HiExprDict [(HiExprValue $ HiValueString $ Text.pack "a", HiExprValue $ HiValueNumber $ 1 % 1)])
        [HiExprValue $ HiValueString $ Text.pack "a"])
