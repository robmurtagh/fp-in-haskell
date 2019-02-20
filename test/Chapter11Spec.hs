module Chapter11Spec where

import           Chapter11
import           Test.Hspec

spec :: Spec
spec = describe "My custom Functor/Applicative/Monad classes" $ do
    it "should have a working implementation on the List type"
        $          myFmap ((+) 1) [1, 2, 3, 4]
        `shouldBe` [2, 3, 4, 5]
    it "should have a working implementation on the Maybe type"
        $          myAp (Just show) (Just [1, 2, 3])
        `shouldBe` Just "[1,2,3]"
    it "should mirror the following example of actual behaviour of ap"
        $          (<*>) [(2 *), (3 *)] [2, 5, 6]
        `shouldBe` [4, 10, 12, 6, 15, 18]
    it "such as in this ap test"
        $          myAp [(2 *), (3 *)] [3, 4, 5]
        `shouldBe` [6, 8, 10, 9, 12, 15]
    it "should have some tests related to working with Maybe as a Monad"
        $          myLookup "A" lookup1
        `shouldBe` (Just "AA")
    it "..." $ myLookup "E" lookup1 `shouldBe` Nothing
    it "..." $ myTripleLookup "A" `shouldBe` Just "AAAA"
    it "..." $ myTripleLookup "F" `shouldBe` Nothing
    it "..." $ myDoTripleLookup "B" `shouldBe` Just "BBBB"
    it "..." $ myDoTripleLookup "F" `shouldBe` Nothing
    it "should have some tests related to working with List as a Monad"
        $          myGenerator ["A", "B"] 3
        `shouldBe` ["A", "A", "A", "B", "B", "B"]
