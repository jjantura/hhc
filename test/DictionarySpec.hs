module DictionarySpec where

import           Crypto.Hash       (MD5 (..), SHA1 (..))
import           Dictionary
import           Test.Hspec

spec :: Spec
spec = do
    describe "MD5.dictionary" $ do
        it "cracks one letter MD5 password with dictionary" $
            dictionary MD5 ["a"] ["0cc175b9c0f1b6a831c399e269772661"] `shouldBe` [("a", "0cc175b9c0f1b6a831c399e269772661")]
        it "cracks seven letter MD5 password with dictionary" $
            dictionary MD5 ["aaaa", "1234567"] ["fcea920f7412b5da7be0cf42b8c93759"] `shouldBe` [("1234567", "fcea920f7412b5da7be0cf42b8c93759")]
    describe "SHA1.dictionary" $ do
        it "cracks one letter SHA1 password with dictionary" $
            dictionary SHA1 ["a"] ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` [("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8")]
        it "cracks seven letter SHA1 password with dictionary" $
            dictionary SHA1 ["aaaa", "1234567"] ["20eabe5d64b0e216796e834f52d61fd0b70332fc"] `shouldBe` [("1234567", "20eabe5d64b0e216796e834f52d61fd0b70332fc")]
