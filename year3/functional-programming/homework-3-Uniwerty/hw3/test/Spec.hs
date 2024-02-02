module Main (
main
) where

import HW3.T1
import HW3.T2
import HW3.T3
import HW3.T4
import Test.Hspec

spec :: Spec
spec = do

--  HW3.T1 tests

  describe "HW3.T1.mapOption" $ do
    it "mapOption id None" $ do
      (mapOption id $ None) `shouldBe` (id None :: Option [Char])

    it "mapOption id Some" $ do
      (mapOption id $ Some "string") `shouldBe` (id $ Some "string" :: Option [Char])

    it "mapOption composion None" $ do
      (mapOption (1+) . mapOption (*3) $ None) `shouldBe` (mapOption ((1+) . (*3)) $ None :: Option Int)

    it "mapOption composion Some" $ do
      (mapOption (1+) . mapOption (*3) $ Some 5) `shouldBe` (mapOption ((1+) . (*3)) $ Some 5 :: Option Int)

  describe "HW3.T1.mapPair" $ do
    it "mapPair id" $ do
      (mapPair id $ P 1 2) `shouldBe` (id $ P 1 2 :: Pair Int)

    it "mapPair composion" $ do
      (mapPair (1+) . mapPair (*3) $ P 1 2) `shouldBe` (mapPair ((1+) . (*3)) $ P 1 2 :: Pair Int)

  describe "HW3.T1.mapQuad" $ do
    it "mapQuad id" $ do
      (mapQuad id $ Q 1 2 3 4) `shouldBe` (id $ Q 1 2 3 4 :: Quad Int)

    it "mapQuad composion" $ do
      (mapQuad (1+) . mapQuad (*3) $ Q 1 2 3 4) `shouldBe` (mapQuad ((1+) . (*3)) $ Q 1 2 3 4 :: Quad Int)

  describe "HW3.T1.mapAnnotated" $ do
    it "mapAnnotated id" $ do
      (mapAnnotated id (1 :# "string")) `shouldBe` (id 1 :# "string" :: Annotated [Char] Int)

    it "mapAnnotated composion" $ do
      (mapAnnotated (1+) . mapAnnotated (*3) $ (1 :# "string"))
      `shouldBe` (mapAnnotated ((1+) . (*3)) (1 :# "string") :: Annotated [Char] Int)

  describe "HW3.T1.mapExcept" $ do
    it "mapExcept id Error" $ do
      (mapExcept id $ Error "error") `shouldBe` (id $ Error "error" :: Except [Char] Int)

    it "mapExcept id Success" $ do
      (mapExcept id $ Success 1) `shouldBe` (id $ Success 1 :: Except [Char] Int)

    it "mapExcept composion Error" $ do
      (mapExcept (1+) . mapExcept (*3) $ Error "error")
      `shouldBe` (mapExcept ((1+) . (*3)) $ Error "error" :: Except [Char] Int)

    it "mapExcept composion Success" $ do
      (mapExcept (1+) . mapExcept (*3) $ Success 1)
      `shouldBe` (mapExcept ((1+) . (*3)) $ Success 1 :: Except [Char] Int)

  describe "HW3.T1.mapPrioritised" $ do
    it "mapPrioritised id" $ do
      (mapPrioritised id $ Low 1) `shouldBe` (id $ Low 1 :: Prioritised Int)

    it "mapPrioritised composion" $ do
      (mapPrioritised (1+) . mapPrioritised (*3) $ High 1)
      `shouldBe` (mapPrioritised ((1+) . (*3)) $ High 1 :: Prioritised Int)

  describe "HW3.T1.mapList" $ do
    it "mapList id" $ do
      (mapList id $ 1 :. 2 :. Nil) `shouldBe` (id $ 1 :. 2 :. Nil :: List Int)

    it "mapList composion" $ do
      (mapList (1+) . mapList (*3) $ 1 :. 2 :. Nil)
      `shouldBe` (mapList ((1+) . (*3)) $ 1 :. 2 :. Nil :: List Int)

  describe "HW3.T1.mapFun" $ do
    it "mapFun id" $ do
      let F f = mapFun id $ F length
      let F g = id $ F length
      (f "hello") `shouldBe` (g "hello" :: Int)

    it "mapFun composion" $ do
      let F f = mapFun (1+) . mapFun (*3) $ F (2-)
      let F g = mapFun ((1+) . (*3)) $ F (2-)
      (f 0) `shouldBe` (g 0 :: Int)

  describe "HW3.T1.mapTree" $ do
    it "mapTree id" $ do
      (mapTree id $ Branch Leaf 1 Leaf) `shouldBe` (id $ Branch Leaf 1 Leaf :: Tree Int)

    it "mapTree composion" $ do
      (mapTree (1+) . mapTree (*3) $ Branch Leaf 1 Leaf)
      `shouldBe` (mapTree ((1+) . (*3)) $ Branch Leaf 1 Leaf :: Tree Int)

--  HW3.T2 tests

  describe "HW3.T2.distF" $ do
    it "distOption" $ do
      (distOption (Some 1, Some 2)) `shouldBe` (Some (1, 2) :: Option (Int, Int))

    it "distPair" $ do
      (distPair (P 1 2, P 3 4)) `shouldBe` (P (1, 3) (2, 4) :: Pair (Int, Int))

    it "distQuad" $ do
      (distQuad (Q 1 2 3 4, Q 5 6 7 8)) `shouldBe` (Q (1, 5) (2, 6) (3, 7) (4, 8) :: Quad (Int, Int))

    it "distAnnotated" $ do
      (distAnnotated (1 :# "first", 2 :# "second"))
      `shouldBe` ((1, 2) :# "firstsecond" :: Annotated [Char] (Int, Int))

    it "distExcept" $ do
      (distExcept (Success 1, Success 2)) `shouldBe` (Success (1, 2) :: Except [Char] (Int, Int))

    it "distPrioritised" $ do
      (distPrioritised (Low 1, High 2)) `shouldBe` (High (1, 2) :: Prioritised (Int, Int))

    it "distList" $ do
      (distList (1 :. 2 :. Nil, "a" :. "b" :. Nil))
      `shouldBe` ((1, "a") :. (1, "b") :. (2, "a") :. (2, "b") :. Nil :: List (Int, [Char]))

    it "distFun" $ do
      let F h = distFun (F (++"!"), F length)
      (h "hello") `shouldBe` (("hello!", 5) :: ([Char], Int))

  describe "HW3.T2.wrapF" $ do
    it "wrapOption" $ do
      (wrapOption 1) `shouldBe` (Some 1 :: Option Int)

    it "wrapPair" $ do
      (wrapPair 1) `shouldBe` (P 1 1 :: Pair Int)

    it "wrapQuad" $ do
      (wrapQuad 1) `shouldBe` (Q 1 1 1 1 :: Quad Int)

    it "wrapAnnotated" $ do
      (wrapAnnotated 1) `shouldBe` (1 :# "" :: Annotated [Char] Int)

    it "wrapExcept" $ do
      (wrapExcept 1) `shouldBe` (Success 1 :: Except Int Int)

    it "wrapPrioritised" $ do
      (wrapPrioritised 1) `shouldBe` (Low 1 :: Prioritised Int)

    it "wrapList" $ do
      (wrapList 1) `shouldBe` (1 :. Nil :: List Int)

    it "wrapFun" $ do
      let F f = wrapFun 1
      (f "anything") `shouldBe` (1 :: Int)

--  HW3.T3 tests

  describe "HW3.T3.joinF" $ do
    it "joinOption None" $ do
      (joinOption $ Some $ None) `shouldBe` (None :: Option Int)

    it "joinOption Some" $ do
      (joinOption $ Some $ Some 1) `shouldBe` (Some 1 :: Option Int)

    it "joinExcept Error" $ do
      (joinExcept $ Success $ Error "error") `shouldBe` (Error "error" :: Except [Char] Int)

    it "joinExcept Success" $ do
      (joinExcept $ Success $ Success 1) `shouldBe` (Success 1 :: Except [Char] Int)

    it "joinAnnotated" $ do
      (joinAnnotated ((1 :# "inner") :# "outer"))
      `shouldBe` (1 :# "outerinner" :: Annotated [Char] Int)

    it "joinList" $ do
      (joinList ((1 :. 2 :. Nil) :. (3 :. Nil) :. Nil))
      `shouldBe` (1 :. 2 :. 3 :. Nil :: List Int)

    it "joinFun" $ do
      let F f = joinFun $ F (\_ -> F $ (\j -> j * 2))
      (f 5) `shouldBe` (5 * 2 :: Int)

--  HW3.T4 tests

  describe "HW3.T4.mapState" $ do
    it "mapState" $ do
      let mState = mapState (*5) (S (\s -> 2 :# s))
      ((runS mState) "mapped") `shouldBe` (2 * 5 :# "mapped" :: Annotated [Char] Int)

  describe "HW3.T4.wrapState" $ do
    it "wrapState" $ do
      let wState = wrapState 2
      ((runS wState) "wrapped") `shouldBe` (2 :# "wrapped" :: Annotated [Char] Int)

  describe "HW3.T4.joinState" $ do
    it "joinState" $ do
      let jState = joinState $ S (\s -> S (\s' -> 2 :# s') :# s)
      ((runS jState) "joint") `shouldBe` (2 :# "joint" :: Annotated [Char] Int)

  describe "HW3.T4.modifyState" $ do
    it "modifyState" $ do
      let mState = modifyState ("now: "++)
      ((runS mState) "modified") `shouldBe` (() :# "now: modified" :: Annotated [Char] ())

  describe "HW3.T4.eval" $ do
    it "eval" $ do
      (runS (eval (2 + 5 * 3 + 9 / (signum (-100)))) [])
      `shouldBe` (8.0 :#
        [
          Add 17.0 (-9.0),
          Div 9.0 (-1.0),
          Sgn (-100.0),
          Sub 0.0 100.0,
          Add 2.0 15.0,
          Mul 5.0 3.0
        ]
        :: Annotated [Prim Double] Double)

main :: IO ()
main = hspec spec
