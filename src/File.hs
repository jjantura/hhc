-- low level file operations
module File
    ( loadFile
    ) where

import           System.IO

loadFile :: FilePath -> IO String
loadFile path = do
    handle <- openFile path ReadMode
    hGetContents handle
