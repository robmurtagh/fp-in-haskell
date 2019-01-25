module Chapter3Spec where

import           Chapter3
import           Test.Hspec

spec :: Spec
spec = describe "MyList" $ do
  it "should be constructable"
    $          (Cons 2 (Cons 1 Empty))
    `shouldBe` (Cons 2 (Cons 1 Empty))
  it "should have a working sum function"
    $          mySum (Cons 8 (Cons 19 Empty))
    `shouldBe` (27 :: Int)
  it "should have a working product"
    $          myProduct (Cons 5 (Cons 8 Empty))
    `shouldBe` (40 :: Int)
  it "should have a working tail function"
    $          myTail (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty)))))
    `shouldBe` (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))
  it "should have a working setHead function"
    $          mySetHead (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))) 9
    `shouldBe` (Cons 9 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty)))))
  it "should have a working drop function"
    $          myDrop (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))) 2
    `shouldBe` (Cons 3 (Cons 4 (Cons 5 Empty)))
  it "should have a working dropWhile function"
    $          myDropWhile (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))) (\x -> x <=3)
    `shouldBe` (Cons 4 (Cons 5 Empty))
  it "should have a working init function"
    $          myInit (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty)))))
    `shouldBe` (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))
