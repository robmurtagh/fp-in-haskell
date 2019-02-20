module Chapter2Spec where

import           Chapter2
import           Test.Hspec

spec :: Spec
spec = do
  describe "fib" $ it "10 should return 34" $ fib 10 `shouldBe` (34 :: Int)
  describe "isSorted" $ do
    it "should return True for a sorted list"
      $          isSorted [1, 2, 3, 4, 5, 5, 6, 9, 999] (<=)
      `shouldBe` (True :: Bool)
    it "should return False for an unsorted list"
      $          isSorted [1, 2, 3, 6, 5, 5, 6, 9, 999] (<=)
      `shouldBe` (False :: Bool)
