module Main where

import HsKu.Config
import HsKu.Load
import HsKu.JSON
import Data.Text.IO as TIO
import Data.ByteString.Lazy as LBS

main :: IO ()
main = do
  cfg   <- loadConfig
  langs <- loadLanguages cfg
  json  <- haikuToJSON langs <$> TIO.getLine
  LBS.putStr json
