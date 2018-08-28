module CharsetSpec where

import           Charset
import           Test.Hspec

spec :: Spec
spec =
    describe "Charset calculations" $ do
        it "calc lower limit for [1-2] lower alpha" $ lowerLimit 26 1 `shouldBe` 0

        it "calc upper limit for [1-2] lower alpha" $ upperLimit 26 1 `shouldBe` 25

        it "calc lower limit for [1-2] lower alpha" $ lowerLimit 26 2 `shouldBe` 26

        it "calc upper limit for [1-2] lower alpha" $ upperLimit 26 2 `shouldBe` 701 -- 26^1 + 26^2 - 1

        it "calc keySpace [1-2] lower alpha" $ keySpace 26 1 2 `shouldBe` 702
