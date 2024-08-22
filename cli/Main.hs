module Main where

import HsKu.Load
import HsKu.JSON
import Data.Text.IO as TIO
import Data.ByteString.Lazy as LBS

main :: IO ()
main = haikuToJSON <$> loadLanguages <*> TIO.getLine >>= LBS.putStr
