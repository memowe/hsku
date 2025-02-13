module Main where

import HsKu.Config
import HsKu.Load
import HsKu.Web
import Data.Map ((!))
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  cfg   <- loadConfig
  langs <- loadLanguages cfg
  let port = read $ cfg ! "webservice" ! "port"
  run port $ hskuWebService cfg langs
