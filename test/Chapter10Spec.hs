module Chapter10Spec where

import           Chapter10
import           Test.Hspec

spec :: Spec
spec = describe "My custom Monoid classes" $ do
    it "should work with my op function"
        $          (op ("Hello" :: String) ("World" :: String))
        `shouldBe` "HelloWorld"
    it "should introduce a Sum which is a monoid under addition"
        $          mappend (Sum 1) (Sum 2)
        `shouldBe` (Sum 3)
    it "should introduce a Sum which is a monoid under addition"
        $          (Sum 1)
        <>         (Sum 2)
        `shouldBe` (Sum 3)
    it "should introduce a Product which is a monoid under multiplication"
        $          (Product 3)
        <>         (Product 6)
        `shouldBe` (Product 18)
    it "should introduce an EndoFunc"
        $          appEndo (EndoFunc ("Hello, " ++) <> EndoFunc (++ "!")) "Rob"
        `shouldBe` "Hello, Rob!"
