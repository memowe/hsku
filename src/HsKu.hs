module HsKu where

import Data.Set (Set)
import Data.Map (Map)

data Language = Language
  { name      :: String
  , vowels    :: Set Char
  , diphtongs :: Set String
  } deriving (Eq, Show)

type Languages = Map String Language
