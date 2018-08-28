module Rules (
    rules,
    _duplicate,
    _reverse,
    _capitalize,
    _lowercase
) where

import Data.Char

rules :: [String -> String]
rules = [_duplicate, _reverse, _capitalize, _lowercase]

_duplicate :: String -> String
_duplicate xs = xs ++ xs

_reverse :: String -> String
_reverse = reverse 

_capitalize :: String -> String
_capitalize = map toUpper 

_lowercase :: String -> String
_lowercase = map toLower


