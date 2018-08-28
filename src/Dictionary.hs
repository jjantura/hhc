module Dictionary

    ( dictionary,
    useDictionary
    ) where

import           CommandLine
import           Common
import           Crypto.Hash        (HashAlgorithm (..), MD5 (..), SHA1 (..),
                                     hashWith)
import           Data.Char
import           Data.List          as L
import           Data.Map           as M
import           Data.Maybe
import           Data.Text          (pack)
import           Data.Text.Encoding (encodeUtf8)
import           File
import           System.Exit
import           Rules


argList :: [String] -> [(Maybe String, Maybe String)]
argList args = L.map (takeArgValue args) ["-a", "-d", "-i", "-m"]

alg2dict :: String -> [String] -> [String] -> [(String, String)]
alg2dict alg =
    let lowercased = L.map toLower alg in
        case lowercased of
            "sha1" -> dictionary SHA1
            _      -> dictionary MD5

useDictionary :: [String] -> IO()
useDictionary args =
                if validateArgs al then do
                    wordlist <- loadFile dictFile
                    content <- loadFile input              
                    print $ alg2dict alg (applyRules $ lines wordlist) (lines content)
                else do
                    putStrLn "Error: incorrect usage. Usage: hhc -m dictionary -a [MD5|SHA1] -d [dictionary] -i [input_file_with_hashes]"
                    exitFailure
            where
                al = argList args
                am = fromList al
                alg = asString $ am ! Just "-a"
                dictFile = asString $ am ! Just "-d"
                input = asString $ am ! Just "-i"
                applyRules = if (asString $ am ! Just "-m") == "rules" then (rules <*>) else ([id] <*>)
                 


-- dictionary, cipher -> (hash, maybe plain text)
dictionary :: HashAlgorithm algorithm => algorithm -> [String] -> [String] -> [(String, String)]
dictionary algorithm words hashes =
  let
    plainToDigest e = show $ hashWith algorithm $ encodeUtf8 $ pack e
    plains = L.foldl' (\a e -> if isJust (elemIndex (plainToDigest e) hashes) then e:a else a) [] words
  in
    zip plains $ L.map plainToDigest plains