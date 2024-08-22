module HsKu.Web where

import HsKu
import HsKu.JSON
import Data.Text
import Servant

type API = "haiku"  :> QueryParam' '[Required] "input" Text
                    :> Get '[JSON] HaikuResult

handler :: Languages -> Text -> Handler HaikuResult
handler langs = return . HaikuResult . fmap snd . parseHaiku langs

hskuWebService :: Languages -> Application
hskuWebService = serve api . handler
  where api = Proxy :: Proxy API
