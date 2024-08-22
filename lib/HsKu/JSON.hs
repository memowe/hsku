module HsKu.JSON where

import HsKu
import Data.Aeson
import Data.Text
import Data.ByteString.Lazy as LBS
import GHC.Generics

newtype HaikuResult = HaikuResult
  { result :: Maybe Haiku
  } deriving Generic

instance ToJSON HaikuResult

haikuToJSON :: Languages -> Text -> LBS.ByteString
haikuToJSON langs input =
  let res = parseHaiku langs input
  in  encode $ HaikuResult (snd <$> res)
