module Main where

import HsKu
import HsKu.Load
import Data.Text
import Data.Aeson
import GHC.Generics
import Servant
import Network.Wai.Handler.Warp

newtype HRes = HRes {result :: Maybe Haiku} deriving Generic
instance ToJSON HRes

type API = "haiku" :> QueryParam' '[Required] "input" Text :> Get '[JSON] HRes

main :: IO ()
main = run 8080 . serve (Proxy :: Proxy API) . handler =<< loadLanguages
  where handler langs = return . HRes . fmap snd . parseHaiku langs
