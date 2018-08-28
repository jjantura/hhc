module Benchmark (
    benchmark
) where

import           Bruteforce
import           Charset         as Cset
import           Control.DeepSeq
import           Crypto.Hash     (MD5 (..), SHA1 (..), SHA224 (..))
import           System.Clock

headers :: [String]
headers = ["method,", "algorithm,", "hashes count,", "keyspace,", "thread count,", "performance[c/s],", "result"]

nanoSecondsInSecond :: Double
nanoSecondsInSecond = 1000000

milliSecondsInSecond :: Double
milliSecondsInSecond = 1000

benchmarkEntry :: String -> String -> [(String, String)] -> IO()
benchmarkEntry method algo res = do
    start <- getTime Monotonic
    let result = res
    end <- result `deepseq` getTime Monotonic
    let timeInMillis = fromIntegral (toNanoSecs $ diffTimeSpec start end) / nanoSecondsInSecond
    let keySpace = Cset.keySpace (length Cset.allPrintableASCIICharset) 1 3
    let performance = fromIntegral keySpace / timeInMillis * milliSecondsInSecond
    putStrLn $ method ++ ", " ++ algo ++ ", 1, " ++ show keySpace ++ ", 1, " ++ show performance

benchmark :: IO()
benchmark = do
    putStrLn $ unwords $ map (++ " ") headers
    benchmarkEntry "bruteforce" "MD5" (bruteforce MD5 Cset.allPrintableASCIICharset 1 3 ["b55e74d4007b674b329d70f5550028ba"])
    benchmarkEntry "bruteforce" "SHA1" (bruteforce SHA1 Cset.allPrintableASCIICharset 1 3 ["045240b6e36beb506efe8d0149db18e7cfca8953"])
    benchmarkEntry "bruteforce" "SHA224" (bruteforce SHA224 Cset.allPrintableASCIICharset 1 3 ["2e6eb13d9425d10eb98454f9dd4f4db14c508e119fca833c91be064e"])



