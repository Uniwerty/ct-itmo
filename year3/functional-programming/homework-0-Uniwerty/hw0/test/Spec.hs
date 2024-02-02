import HW0.T1
import HW0.T3
import HW0.T4
import HW0.T5
import Numeric.Natural (Natural)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "HW0.T1" $ do
    it "distrib" $ do
      distrib (Left 1) `shouldBe` ((Left 1, Left 1) :: (Either Int Int, Either Int Int))
      distrib (Right (1, 'a')) `shouldBe` ((Right 1, Right 'a') :: (Either Int Int, Either Int Char))

    it "flipIso & runIso" $ do
      runIso (flipIso $ Iso (*2) (/2)) 2 `shouldBe` (1 :: Double)

    it "assocPair" $ do
      runIso assocPair (1, (2, 3)) `shouldBe` (((1, 2), 3) :: ((Int, Int), Int))

    it "assocEither" $ do
      runIso assocEither (Left 1) `shouldBe` (Left (Left 1) :: Either (Either Int Char) Char)
      runIso assocEither (Right (Left 1)) `shouldBe` (Left (Right 1) :: Either (Either Char Int) Char)
      runIso assocEither (Right (Right 1)) `shouldBe` (Right 1 :: Either (Either Char Char) Int)

  describe "HW0.T3" $ do
    it "S combinator" $ do
      s (-) (*2) 5 `shouldBe` (-5 :: Int)

    it "K combinator" $ do
      k 1 'a' `shouldBe` (1 :: Int)

    it "I combinator" $ do
      i 1 `shouldBe` (1 :: Int)

    it "B combinator (compose)" $ do
      compose (*2) (+3) 1 `shouldBe` (8 :: Int)

    it "W combinator (contract)" $ do
      contract (\a b -> [a, b]) 'x' `shouldBe` ("xx" :: String)

    it "C combinator (permute)" $ do
      permute (\a b -> [a, b]) 'x' 'y' `shouldBe` ("yx" :: String)

  describe "HW0.T4" $ do
    it "repeat'" $ do
      head (tail (tail (repeat' 1))) `shouldBe` (1 :: Int)

    it "map'" $ do
      map' (+1) [1, 2, 3, 4, 5] `shouldBe` ([2, 3, 4, 5, 6] :: [Int])

    it "fib" $ do
      fib 0 `shouldBe` (0 :: Natural)
      fib 1 `shouldBe` (1 :: Natural)
      fib 3 `shouldBe` (2 :: Natural)
      fib 7 `shouldBe` (13 :: Natural)
      fib 100 `shouldBe` (354224848179261915075 :: Natural)

    it "fac" $ do
      fac 0 `shouldBe` (1 :: Natural)
      fac 1 `shouldBe` (1 :: Natural)
      fac 3 `shouldBe` (6 :: Natural)
      fac 5 `shouldBe` (120 :: Natural)
      fac 20 `shouldBe` (2432902008176640000 :: Natural)

  describe "HW0.T5" $ do
    it "nToNum" $ do
      nToNum nz `shouldBe` (0 :: Natural)
      nToNum (ns nz) `shouldBe` (1 :: Natural)
      nToNum (ns (ns (ns nz))) `shouldBe` (3 :: Natural)

    it "nFromNatural" $ do
      nToNum (nFromNatural 0) `shouldBe` (0 :: Natural)
      nToNum (nFromNatural 55) `shouldBe` (55 :: Natural)

    it "nplus" $ do
      nToNum (nplus nz nz) `shouldBe` (0 :: Natural)
      nToNum (nplus (ns nz) nz) `shouldBe` (1 :: Natural)
      nToNum (nplus (ns nz) (ns nz)) `shouldBe` (2 :: Natural)
      nToNum (nplus (nFromNatural 49) (nFromNatural 51)) `shouldBe` (100 :: Natural)

    it "nmult" $ do
      nToNum (nmult nz nz) `shouldBe` (0 :: Natural)
      nToNum (nmult (ns nz) nz) `shouldBe` (0 :: Natural)
      nToNum (nmult (ns nz) (ns nz)) `shouldBe` (1 :: Natural)
      nToNum (nmult (nFromNatural 25) (nFromNatural 25)) `shouldBe` (625 :: Natural)
