module Bruteforce

    ( useBruteforce,
      bruteforce
    ) where

import           Charset
import           CommandLine
import           Common
import           Crypto.Hash        (HashAlgorithm (..), MD5 (..), SHA1 (..), SHA224 (..),
                                     hashWith)
import           Data.Char
import           Data.List          as L
import           Data.Map           as M
import           Data.Maybe
import           Data.Text          (pack)
import           Data.Text.Encoding (encodeUtf8)
import           File
import           System.Exit


requiredArgList :: [String] -> [(Maybe String, Maybe String)]
requiredArgList args = L.map (takeArgValue args) ["-a", "-c", "-ll", "-ul", "-i"]

optionalArgList :: [String] -> [(Maybe String, Maybe String)]
optionalArgList args = L.map (takeArgValue args) ["-t"]

alg2bf :: String -> String -> Int -> Int -> [String] -> [(String, String)]
alg2bf alg =
    let lowercased = L.map toLower alg in
        case lowercased of            
            "md5"  -> bruteforce MD5
            "sha1" -> bruteforce SHA1
            "sha224" -> bruteforce SHA224


useBruteforce :: [String] -> IO()
useBruteforce args =
                if validateArgs al then do
                    content <- loadFile input
                    print $ alg2bf alg charset minLen maxLen $ lines content
                else do
                    putStrLn "Error: incorrect usage. Usage: hhc -m bruteforce -a [MD5|SHA1] -c [charset] -ll [lower length limit] -ul [upper length limit] -i [input_file_with_hashes]"
                    exitFailure
            where
                al = requiredArgList args
                am = fromList al
                alg = asString $ am ! Just "-a"
                charset = asString $ am ! Just "-c"
                minLen = asInt $ am ! Just "-ll"
                maxLen = asInt $ am ! Just "-ul"
                input = asString $ am ! Just "-i"

bruteforce :: HashAlgorithm algorithm => algorithm -> String -> Int -> Int -> [String] -> [(String, String)]
bruteforce algorithm charset minl maxl = bruteforceP algorithm charset (lowerLimit (length charset) minl) (upperLimit (length charset) maxl) 

-- charset, min len, max len, cipher -> (hash, maybe plain text)
bruteforceP :: HashAlgorithm algorithm => algorithm -> String -> Int -> Int -> [String] -> [(String, String)]
bruteforceP algorithm charset lower upper hashes =
  let
    plainToDigest e = show $ hashWith algorithm $ encodeUtf8 $ pack e
    digest e = plainToDigest $ genPlain charset e
    foundIndices = L.foldl' (\a e -> if isJust (elemIndex (digest e) hashes) then e:a else a) [] [lower .. upper]
    plains = L.map (genPlain charset) foundIndices
  in
    zip plains $ L.map plainToDigest plains
    