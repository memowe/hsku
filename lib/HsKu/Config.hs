module HsKu.Config where

import Data.List
import Data.Maybe
import Data.Map (Map, assocs, fromList)
import Text.ParserCombinators.ReadP
import System.Environment

type    IniMap  = Map String (Map String String)
newtype Ini     = Ini {sections :: IniMap} deriving Eq

instance Show Ini where
  show = unlines . map section . assocs . sections
    where section (name, sec) = "[" ++ name ++ "]\n" ++ pairs sec
          pairs               = unlines . map pair . assocs
          pair (k, v)         = k ++ " = " ++ v

instance Read Ini where
  readsPrec _ = readP_to_S parser
    where parser  = Ini . fromList <$> many section
          section = do  name  <- trim <$> between (char '[')
                                                  (char ']' >> nls)
                                                  (no "=\n]")
                        pairs <- many pair
                        return (name, fromList pairs)
          pair    = do  key <- trim <$> no "\n[="
                        val <- trim <$> between (char '=') nls (no "\n")
                        return (key, val)
          nls     = munch1 (== '\n')
          no      = munch1 . flip notElem :: String -> ReadP String
          trim    = dropWhile (== ' ') . dropWhileEnd (== ' ')

loadConfig :: IO IniMap
loadConfig = do
  configFileName <- fromMaybe "config.ini" <$> lookupEnv "HSKU_CONFIG_FILE"
  sections . read <$> readFile configFileName
