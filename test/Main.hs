module Main (main) where

import            Test.Hspec
import            System.IO.Temp

import            HsKu
import            HsKu.Load
import qualified  Data.Set as S
import qualified  Data.Map as M
import            System.FilePath
import            System.Environment

main :: IO ()
main = hspec $ describe "HsKu tests" $ do

  context "Language loading" $ do
    langs <- runIO $ withSystemTempDirectory "hsku-languages" $ \dir -> do
              writeFile (dir </> "foo.yml")
                "name: foo\nvowels: a b c\ndiphtongs: de fg\n"
              writeFile (dir </> "bar.yml")
                "name: bar\nvowels: h i j\ndiphtongs: kl mn\n"
              setEnv "HSKU_LANGUAGES" dir
              loadLanguages
    it "Correct languages parsed" $
      langs `shouldBe` M.fromList
        [ ("foo", Language  { name      = "foo"
                            , vowels    = S.fromList "abc"
                            , diphtongs = S.fromList ["de", "fg"]
                            })
        , ("bar", Language  { name      = "bar"
                            , vowels    = S.fromList "hij"
                            , diphtongs = S.fromList ["kl", "mn"]
                            })
        ]
