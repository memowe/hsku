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

  context "Haiku parsing" $ do
    let german = Language { name      = "Deutsch"
                          , vowels    = S.fromList "aeou"
                          , diphtongs = S.fromList ["au", "ei"]
                          }
    it "Reject nonsense" $
      parseHaiku german "Die Antwort ist 42!" `shouldBe` Nothing
    it "Correctly split Haiku" $
      parseHaiku german "Decken auf dem Gras, eine Nacht lang ohne Haus - reich nur durch den Mond."
        `shouldBe` Just ( "Decken auf dem Gras,"
                        , "eine Nacht lang ohne Haus -"
                        , "reich nur durch den Mond."
                        )
