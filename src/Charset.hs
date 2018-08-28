module Charset
    (   lowerAlphaCharset,
        upperAlphaCharset,
        digitsCharset,
        specialCharset,
        allPrintableASCIICharset,
        genPlain,
        maybeGenPlain,
        lowerLimit,
        upperLimit,
        keySpace
    ) where

import           Data.Maybe

lowerAlphaCharset :: String
lowerAlphaCharset = "qazwsxedcrfvtgbyhnujmikolp"

upperAlphaCharset :: String
upperAlphaCharset = "QAZWSXEDCRFVTGBYHNUJMIKOLP"

digitsCharset :: String
digitsCharset = "0123456789"

specialCharset :: String
specialCharset = "~`!@#$%^&*()_+|-=\\{}[]:\";'<>?,./ "

allPrintableASCIICharset :: String
allPrintableASCIICharset = specialCharset ++ digitsCharset ++ lowerAlphaCharset ++ upperAlphaCharset

-- charset, index -> plain text
maybeGenPlain :: String -> Maybe Int -> Maybe String
maybeGenPlain c i = if isJust i then Just (genPlain c $ fromJust i) else Nothing

-- charset, index -> plain text
genPlain :: String -> Int -> String
genPlain c i =
  if i < length c then getchar else prefix ++ getchar
    where
        qr = i `divMod` length c
        prefix = genPlain c (fst qr - 1)
        getchar = [c !! snd qr]


keySpace :: Int -> Int -> Int -> Int
keySpace charsetLen minLen maxLen = sum [ charsetLen ^ x | x <- [minLen..maxLen]]

lowerLimit :: Int -> Int -> Int
lowerLimit charsetLen plainLen = sum [ charsetLen ^ x | x <- [1..plainLen - 1]]

upperLimit :: Int -> Int -> Int
upperLimit charsetLen plainLen = sum [ charsetLen ^ x | x <- [1..plainLen]] - 1

