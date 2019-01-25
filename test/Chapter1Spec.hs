module Chapter1Spec where

import Chapter1
import Test.Hspec

spec :: Spec
spec = do
  describe "fib" $ do
    it "10 should return 34" $ do
      fib 10 `shouldBe` (34 :: Int)

  describe "isSorted" $ do
    it "should return True for a sorted list" $ do
      isSorted [1,2,3,4,5,5,6,9,999] (\x y -> x <= y) `shouldBe` (True :: Bool)
    it "should return False for an unsorted list" $ do
      isSorted [1,2,3,6,5,5,6,9,999] (\x y -> x <= y) `shouldBe` (False :: Bool)