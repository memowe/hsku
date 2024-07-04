module Main where

import HsKu.Load
import HsKu.Web
import Network.Wai.Handler.Warp

main :: IO ()
main = loadLanguages >>= run 8080 . hskuWebService
