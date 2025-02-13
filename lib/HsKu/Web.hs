module HsKu.Web where

import HsKu
import HsKu.Config
import HsKu.JSON
import qualified Data.Text as T
import Data.Text (Text)
import Data.String
import Data.Map ((!))
import Data.Aeson
import Servant as S
import Network.HTTP.Simple
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics

type Name     = Text
type Channel  = Text
data MMsg     = MMsg
  { text          :: Text
  , user_name     :: Name
  , channel_name  :: Channel
  } deriving (Eq, Show, Generic)

instance FromJSON MMsg

data MPost = MPost
  { chn :: Channel
  , msg :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON MPost where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = flm}
    where flm "msg" = "text"
          flm "chn" = "channel_name"
          flm other = other

praise :: Name -> Channel -> Haiku -> MPost
praise n c (h1,h2,h3) = MPost c $ T.unlines $
  ("@" <> n <> " 🥳 Haiku gefunden!\n") : map ("> " <>) [h1,h2,h3]

type API = "haiku"        :> QueryParam' '[Required] "input" Text
                          :> Get '[JSON] HaikuResult
      :<|>  "mattermost"  :> ReqBody '[JSON] MMsg :> PostNoContent

server :: Config -> Languages -> Server API
server cfg langs = handleHaiku :<|> handleMattermost
  where

      handleHaiku :: Text -> Handler HaikuResult
      handleHaiku t = do
        let mhaiku = snd <$> parseHaiku langs t
        return $ HaikuResult mhaiku

      handleMattermost :: MMsg -> Handler NoContent
      handleMattermost m = do
        whenMaybeM (snd <$> parseHaiku langs (text m)) $ \haiku -> do
          let jpraise = praise (user_name m) (channel_name m) haiku
              request = fromString $ cfg ! "mattermost" ! "post-url"
              postReq = setRequestMethod "POST" request
              jsonReq = setRequestBodyJSON jpraise postReq
          void $ liftIO (httpNoBody jsonReq)
        return NoContent
        where whenMaybeM = flip $ maybe $ return ()

hskuWebService :: Config -> Languages -> Application
hskuWebService cfg langs = serve api $ server cfg langs
  where api = S.Proxy :: S.Proxy API
