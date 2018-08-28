module BruteforceSpec where

import           Bruteforce
import           Crypto.Hash       (MD5 (..), SHA1 (..), SHA224 (..))
import           Test.Hspec

spec :: Spec
spec = do
    describe "MD5.bruteforce" $ do
        it "cracks one letter MD5 password for range [1-1]" $
            bruteforce MD5 "asdfghjkl" 1 1 ["0cc175b9c0f1b6a831c399e269772661"] `shouldBe` [("a", "0cc175b9c0f1b6a831c399e269772661")]

        it "cracks one letter MD5 password for range [1-2]" $
            bruteforce MD5 "asdfghjkl" 1 2 ["0cc175b9c0f1b6a831c399e269772661"] `shouldBe` [("a", "0cc175b9c0f1b6a831c399e269772661" )]

        it "cracks two letters MD5 password for range [1-1]" $
            bruteforce MD5 "asdfghjkl" 1 1 ["4124bc0a9335c27f086f24ba207a4912"] `shouldBe` []

        it "cracks two letters MD5 password for range [1-2]" $
            bruteforce MD5 "asdfghjkl" 1 2 ["4124bc0a9335c27f086f24ba207a4912"] `shouldBe` [("aa", "4124bc0a9335c27f086f24ba207a4912")]
    
    describe "SHA1.bruteforce" $ do
        it "cracks one letter SHA1 password for range [1-1]" $
            bruteforce SHA1 "asdfghjkl" 1 1 ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` [("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8")]

        it "cracks one letter SHA1 password for range [1-2]" $
            bruteforce SHA1 "asdfghjkl" 1 2 ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` [("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8" )]

        it "cracks two letters SHA1 password for range [1-1]" $
            bruteforce SHA1 "asdfghjkl" 1 1 ["e0c9035898dd52fc65c41454cec9c4d2611bfb37"] `shouldBe` []

        it "cracks two letters SHA1  password for range [1-2]" $
            bruteforce SHA1 "asdfghjkl" 1 2 ["e0c9035898dd52fc65c41454cec9c4d2611bfb37"] `shouldBe` [("aa", "e0c9035898dd52fc65c41454cec9c4d2611bfb37")]

        it "returns empty string for _invalid_ charset" $
            bruteforce SHA1 "123456" 1 1 ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` []

    describe "SHA224.bruteforce" $ do
        it "cracks one letter SHA224 password for range [1-1]" $
            bruteforce SHA224 "asdfghjkl" 1 1 ["abd37534c7d9a2efb9465de931cd7055ffdb8879563ae98078d6d6d5"] `shouldBe` [("a", "abd37534c7d9a2efb9465de931cd7055ffdb8879563ae98078d6d6d5")]

        it "cracks one letter SHA224 password for range [1-2]" $
            bruteforce SHA224 "asdfghjkl" 1 2 ["abd37534c7d9a2efb9465de931cd7055ffdb8879563ae98078d6d6d5"] `shouldBe` [("a", "abd37534c7d9a2efb9465de931cd7055ffdb8879563ae98078d6d6d5" )]

        it "cracks two letters SHA224 password for range [1-1]" $
            bruteforce SHA224 "asdfghjkl" 1 1 ["2ef29e646f6de95de993a59eb1a94cbf52986892949a0abc015a01f7"] `shouldBe` []

        it "cracks two letters SHA224  password for range [1-2]" $
            bruteforce SHA224 "asdfghjkl" 1 2 ["2ef29e646f6de95de993a59eb1a94cbf52986892949a0abc015a01f7"] `shouldBe` [("aa", "2ef29e646f6de95de993a59eb1a94cbf52986892949a0abc015a01f7")]

        it "returns empty string for _invalid_ charset" $
            bruteforce SHA224 "123456" 1 1 ["2ef29e646f6de95de993a59eb1a94cbf52986892949a0abc015a01f7"] `shouldBe` []
        
        
        
