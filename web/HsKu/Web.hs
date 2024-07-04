module HsKu.Web where

import HsKu
import Data.Text
import Data.Aeson
import GHC.Generics
import Servant

newtype HRes = HRes {result :: Maybe Haiku} deriving Generic
instance ToJSON HRes

type API = "haiku" :> QueryParam' '[Required] "input" Text :> Get '[JSON] HRes

handler :: Languages -> Text -> Handler HRes
handler langs = return . HRes . fmap snd . parseHaiku langs

hskuWebService :: Languages -> Application
hskuWebService = serve api . handler
  where api = Proxy :: Proxy API
