module Common (
    asInt,
    asString,
    validateArgs
) where

import           Data.List  as L
import           Data.Maybe

asInt :: Maybe String -> Int
asInt s = if isJust s then read $ fromJust s :: Int else -1

asString :: Maybe String -> String
asString = fromMaybe ""

validateArgs :: [(Maybe String, Maybe String)] -> Bool
validateArgs = L.foldl (\a e -> if isNothing $ snd e then a && False else a) True
