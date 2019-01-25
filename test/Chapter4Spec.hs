module Chapter4Spec where

import           Chapter4
import           Test.Hspec

spec :: Spec
spec = describe "MyOption" $ do
    it "should be constructable"
        $ Some "Hello" `shouldBe` Some "Hello"
    it "should map with None handling"
        $ myMap None (\x -> x + 1) `shouldBe` None
    it "should map over Some value"
        $ myMap (Some 11) (\x -> x + 1) `shouldBe` Some 12