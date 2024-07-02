module HsKu where

import Data.Bool
import Data.Char
import Data.Text as T
import Data.Set as S
import Data.Map
import Data.Void
import Data.Either.Extra
import Text.Megaparsec as P
import Text.Megaparsec.Char
import Control.Applicative as A
import Control.Monad.Reader

data Language = Language
  { name      :: Text
  , vowels    :: Set Char
  , diphtongs :: Set Text
  } deriving (Eq, Show)

type Languages = Map Text Language

sylChars :: Language -> Set Char
sylChars = S.union <$> vowels <*> S.map T.head . diphtongs

type Haiku = (Text, Text, Text)

type Parser = ParsecT Void Text (Reader Language)

pVowel :: Parser Char
pVowel = do
  vs <- asks (S.toList . vowels)
  choice (char' <$> vs)

pDiphtong :: Parser Text
pDiphtong = do
  dps <- asks (S.toList . diphtongs)
  choice (string' <$> dps)

pSyl :: Parser Text
pSyl = try pDiphtong <|> (T.singleton <$> pVowel)

pSep :: Parser Text
pSep = do
  syls <- asks sylChars
  takeWhile1P Nothing (not . (`S.member` syls))

pWordSep :: Parser Text
pWordSep = pSep >>= guarded (T.any isSpace)

pSylSep :: Parser (Text, Text)
pSylSep = (,) <$> pSyl <*> pSep

pSyls :: Int -> Parser Text
pSyls n = do
  sylSeps <- P.count (n-1) pSylSep
  lastSyl <- pSyl
  let initSyls = T.concat (uncurry append <$> sylSeps)
  return $ initSyls <> lastSyl

pHaiku :: Parser Haiku
pHaiku = do
  start1          <- option "" pSep
  line1           <- pSyls 5
  (end1, start2)  <- splitWordSep <$> pWordSep
  line2           <- pSyls 7
  (end2, start3)  <- splitWordSep <$> pWordSep
  line3           <- pSyls 5
  end3            <- option "" pSep
  return  ( start1 <> line1 <> end1
          , start2 <> line2 <> end2
          , start3 <> line3 <> end3
          )

parseHaiku :: Language -> Text -> Maybe Haiku
parseHaiku lang = eitherToMaybe . flip runReader lang . runParserT pHaiku ""

-- Utility functions

splitWordSep :: Text -> (Text, Text)
splitWordSep sep =
  let right = T.takeWhileEnd (not.isSpace) sep
      rest  = T.dropWhileEnd (not.isSpace) sep
      left  = T.dropWhileEnd isSpace rest
  in  (left, right)

-- soon to be in Control.Monad.Extra
guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded = liftA2 (bool A.empty) pure
