{-# OPTIONS_GHC -Wno-orphans #-}
module HsKu.Load where

import            HsKu
import            Data.Maybe
import            Data.Either
import qualified  Data.Set as S
import qualified  Data.Map as M
import            Data.Yaml
import            System.Environment
import            System.Directory
import            System.FilePath
import            Control.Monad

instance FromJSON Language where
  parseJSON (Object v) = Language
    <$>                   (v .: "name")
    <*> fmap toVowels     (v .: "vowels")
    <*> fmap toDiphtongs  (v .: "diphtongs")
    where toVowels    = S.fromList . map head . words
          toDiphtongs = S.fromList . words
  parseJSON _ = fail "Couldn't parse language"

loadLanguages :: IO Languages
loadLanguages = do
  langDir   <- fromMaybe "languages" <$> lookupEnv "HSKU_LANGUAGES"
  langFiles <- filter ((== ".yml") . takeExtension) <$> listDirectory langDir
  nameLangs <- fmap rights $ forM langFiles $ \fp -> do
                let ln = takeBaseName fp
                fmap (ln,) <$> decodeFileEither (langDir </> fp)
  return $ M.fromList nameLangs
