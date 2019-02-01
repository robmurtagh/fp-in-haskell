module Chapter4Spec where

import           Chapter4
import           Test.Hspec

spec :: Spec
spec = describe "MyOption" $ do
    it "should be constructable" $ Some "Hello" `shouldBe` Some "Hello"
    it "should map with None handling"
        $          myMap None (\x -> x + 1)
        `shouldBe` None
    it "should map over Some value"
        $          myMap (Some 11) (\x -> x + 1)
        `shouldBe` Some 12
    it "should have a getOrElse accessor handling Some"
        $          myGetOrElse (Some 11) 12
        `shouldBe` 11
    it "should have a getOrElse accessor handling None"
        $          myGetOrElse None 12
        `shouldBe` 12
    it "should have a working filter function"
        $          myFilter (Some 5) ((==) 5)
        `shouldBe` Some 5
    it "should have a working filter function"
        $          myFilter (Some 5) ((==) 4)
        `shouldBe` None
    it "should have a variance implementation using flatMap"
        $          myVariance [1, 5, 7, 3, 4, 4, 2, 3, 5, 7, 4]
        `shouldBe` Some 3.173553719008264
    it "should have a map2 implementation"
        $          myMap2 (Some 5) (Some 6) (+)
        `shouldBe` Some 11
    it "should have a sequence implementation"
        $          mySequence [(Some 5), (Some 6), (Some 7)]
        `shouldBe` Some [5, 6, 7]
    it "should have a traverse implementation"
        $          myTraverse [1, 2, 3, 4, 5] (\x -> Some (x + 1))
        `shouldBe` Some [2, 3, 4, 5, 6]

