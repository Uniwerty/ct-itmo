module Main (
main
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Sum (..))
import HW2.T1
import HW2.T2
import HW2.T3
import HW2.T4
import Test.Hspec

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []

spec :: Spec
spec = do
  describe "HW2.T1.tfoldr" $ do
      let tree = (Branch 3 (Branch 1 Leaf 1 Leaf) 2 (Branch 1 Leaf 5 Leaf))

      it "Empty tree to empty list" $ do
        treeToList Leaf `shouldBe` ([] :: [Int])

      it "Singleton tree to list" $ do
        treeToList (Branch 1 Leaf 0 Leaf) `shouldBe` ([0] :: [Int])

      it "Tree to sorted list" $ do
        treeToList tree `shouldBe` ([1, 2, 5] :: [Int])

      it "Tree sum" $ do
        tfoldr (+) 0 tree `shouldBe` (1 + (2 + (5 + 0)) :: Int)

      it "Tree subtraction" $ do
        tfoldr (-) 0 tree `shouldBe` (1 - (2 - (5 - 0)) :: Int)

  describe "HW2.T2.splitOn" $ do
    it "Split empty list" $ do
      splitOn 0 [] `shouldBe` ([] :| [] :: NonEmpty [Int])

    it "Split list" $ do
      splitOn 0 [1, 2, 0, 3, 0, 4] `shouldBe` ([1, 2] :| [[3], [4]] :: NonEmpty [Int])

    it "Split list with separator at the end" $ do
      splitOn 0 [1, 2, 0, 3, 0] `shouldBe` ([1, 2] :| [[3], []] :: NonEmpty [Int])

    it "Split string" $ do
      splitOn '/' "path/to/file.exe" `shouldBe` ("path" :| ["to", "file.exe"] :: NonEmpty [Char])

    it "Split string with separator at the end" $ do
      splitOn '/' "path/to/directory/" `shouldBe` ("path" :| ["to", "directory", ""] :: NonEmpty [Char])

  describe "HW2.T2.joinWith" $ do
    it "Join empty lists" $ do
      joinWith 0 ([] :| []) `shouldBe` ([] :: [Int])

    it "Join lists" $ do
      joinWith 0 ([1] :| [[2], [3, 4]]) `shouldBe` ([1, 0, 2, 0, 3, 4] :: [Int])

    it "Join lists with separator at the end" $ do
      joinWith 0 ([1] :| [[2, 3], []]) `shouldBe` ([1, 0, 2, 3, 0] :: [Int])

    it "Join strings" $ do
      joinWith '-' ("mojo" :| ["dojo", "casa", "house"]) `shouldBe` ("mojo-dojo-casa-house" :: [Char])

    it "Join strings with separator at the end" $ do
      joinWith '.' ("Hi" :| ["How", "Are", "You", ""]) `shouldBe` ("Hi.How.Are.You." :: [Char])

  describe "HW2.T3.mcat" $ do
    it "Mcat nothing" $ do
      mcat [Nothing, Nothing] `shouldBe` ([] :: [Char])

    it "Mcat just" $ do
      mcat [Just "mo", Just "", Just "no", Just "id"] `shouldBe` ("monoid" :: [Char])

    it "Mcat both" $ do
      mcat [Nothing, Just "mo", Just "no", Nothing, Just "id", Nothing] `shouldBe` ("monoid" :: [Char])

  describe "HW2.T3.epart" $ do
    it "Epart only left" $ do
      epart [Left "", Left "mono", Left "id"] `shouldBe` (("monoid", "") :: ([Char], [Char]))

    it "Epart ints and strings" $ do
      epart [Left (Sum 5), Left (Sum 1), Right "foo", Left (Sum 1), Right "bar"]
      `shouldBe` ((Sum 7, "foobar") :: (Sum Int, [Char]))

  describe "HW2.T4.ListPlus" $ do
    it "ListPlus mappend" $ do
      (1 :+ Last 2) <> (3 :+ Last 4) `shouldBe` (1 :+ 2 :+ 3 :+ Last 4 :: ListPlus Int)

    it "ListPlus associativity" $ do
      ((1 :+ Last 2) <> Last 4) <> (5 :+ Last 6)
      `shouldBe` ((1 :+ Last 2) <> (Last 4 <> (5 :+ Last 6)) :: ListPlus Int)

  describe "HW2.T4.Inclusive" $ do
    it "Only This mappend" $ do
      This "foo" <> This "bar" `shouldBe` (This "foobar" :: Inclusive [Char] [Char])

    it "Only That mappend" $ do
      That "foo" <> That "bar" `shouldBe` (That "foobar" :: Inclusive [Char] [Char])

    it "Both mappend" $ do
      This "fun" <> Both "cti" "prog" <> That "ram" <> That "min" <> Both "on" "g" <> This "al"
      `shouldBe` (Both "functional" "programming" :: Inclusive [Char] [Char])

    it "Inclusive associativity" $ do
      (This "a" <> That "b") <> Both "c" "c"
      `shouldBe` (This "a" <> (That "b" <> Both "c" "c") :: Inclusive [Char] [Char])

  describe "HW2.T4.DotString" $ do
    it "Empty DotString mappend" $ do
      mempty <> DS "aba" <> mempty `shouldBe` (DS "aba" :: DotString)

    it "DotString mappend" $ do
      DS "mo" <> mempty <> DS "no" <> DS "id" <> mempty `shouldBe` (DS "mo.no.id" :: DotString)

    it "DotString associativity" $ do
      (DS "mo" <> DS "no") <> DS "id" `shouldBe` (DS "mo" <> (DS "no" <> DS "id") :: DotString)

  describe "HW2.T4.Fun" $ do
    it "Fun Int -> Int mappend" $ do
      let F fun = F (1+) <> mempty <> F (*2)
      fun 5 `shouldBe` (11 :: Int)

    it "Fun [Char] -> [Char] mappend" $ do
      let F fun = mempty <> F ("my-"++) <> F (++".exe")
      fun "program" `shouldBe` ("my-program.exe" :: [Char])

main :: IO ()
main = hspec spec
