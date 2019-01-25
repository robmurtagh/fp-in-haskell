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
    $          myDropWhile (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty)))))
                           (\x -> x <= 3)
    `shouldBe` (Cons 4 (Cons 5 Empty))
  it "should have a working init function"
    $          myInit (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty)))))
    `shouldBe` (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))
  it "should have a working foldRight function"
    $ myFoldRight (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))) 0 (+)
    `shouldBe` (15 :: Int)
  it "should have a working length function implemented using foldRight"
    $          myLength (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty)))))
    `shouldBe` (5 :: Int)
  it "should have a working reverse function implemented using a fold"
    $          myReverse (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty)))))
    `shouldBe` (Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Empty)))))

  describe "MyTree" $ do
    it "should be constructable"
      $          Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3)) `shouldBe` Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
    it "should have a working size function"
      $          mySize(Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) `shouldBe` (3 :: Int)
    it "should have a working max function"
      $          myMax(Branch (Leaf (-15)) (Branch (Leaf 223) (Leaf 33))) `shouldBe` (223 :: Int)
    it "should have a working depth function"
      $          myDepth(Branch (Leaf (-15)) (Branch (Leaf 223) (Branch (Leaf 45) (Leaf 33)))) `shouldBe` (3 :: Int)
    it "should have a working map function"
      $          myMap(Branch (Leaf (-15)) (Branch (Leaf 223) (Branch (Leaf 45) (Leaf 33)))) ((+) 1) 
      `shouldBe` Branch (Leaf (-14)) (Branch (Leaf 224) (Branch (Leaf 46) (Leaf 34)))