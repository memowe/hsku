module HsKu where

import Data.Text
import Data.Set (Set)
import Data.Map (Map)

data Language = Language
  { name      :: Text
  , vowels    :: Set Char
  , diphtongs :: Set Text
  } deriving (Eq, Show)

type Languages = Map Text Language
