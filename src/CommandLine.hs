module CommandLine (
    takeArgValue
) where

import           Data.List
import           Data.Maybe

 -- TODO: this code is ugly and unreadable, replace with applicative optparse asap
takeArgValue :: [String] -> String -> (Maybe String, Maybe String)
takeArgValue args arg =
    let maybeArgIndex = arg `elemIndex` args in
    let index = fromJust maybeArgIndex in
    let argsCount = length args in
        if isJust maybeArgIndex && index + 1 < argsCount then
             (Just arg, Just (args !! succ index)) else (Just arg, Nothing)
