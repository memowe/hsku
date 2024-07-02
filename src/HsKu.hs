module HsKu where

import Data.Bool
import Data.Char
import Data.Text as T
import Data.Set as S
import Data.Map
import Data.Void
import Text.Megaparsec
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

type Parser = ParsecT Void Text (Reader Language)

pVowel :: Parser Char
pVowel = asks vowels >>= oneOf

pDiphtong :: Parser Text
pDiphtong = do
  dps <- asks (S.toList . diphtongs)
  choice (chunk <$> dps)

pSep :: Parser Text
pSep = do
  syls <- asks sylChars
  takeWhile1P Nothing (not . (`S.member` syls))

pWordSep :: Parser Text
pWordSep = pSep >>= guarded (T.any isSpace)



-- Utility function, soon to be in Control.Monad.Extra
guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded = liftA2 (bool A.empty) pure
