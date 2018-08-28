{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe
import           System.Environment
import           System.Exit

import           Benchmark
import           Bruteforce
import           Dictionary

import           CommandLine

dispatch :: [String] -> IO()
dispatch args =
    let incorrectUsage = putStrLn "incorrect usage, use -m switch \n"
        maybeMode = snd $ takeArgValue args "-m" in
            if isJust maybeMode then do
                case fromJust maybeMode of
                    "benchmark"  -> benchmark
                    "bruteforce" -> useBruteforce args
                    "dictionary" -> useDictionary args
                    "rules"      -> useDictionary args
                    _            -> putStrLn "Unknown mode"
                exitSuccess
            else do
                incorrectUsage
                exitFailure

main :: IO ()
main = do
    args <- getArgs
    dispatch args
