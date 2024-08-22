module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import System.IO.Temp

import HsKu
import HsKu.Load
import HsKu.Web
import Data.Text
import Data.Set as S
import Data.Map as M
import System.FilePath
import System.Environment
import Network.URI.Encode
import HsKu.JSON (haikuToJSON)

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
    let german  = Language  { name      = "Deutsch"
                            , vowels    = S.fromList "aeou"
                            , diphtongs = S.fromList ["au", "ei"]
                            }
        fooLang = Language  { name      = "Foo Language"
                            , vowels    = S.fromList "ao"
                            , diphtongs = S.fromList ["oo", "uu"]
                            }
        langs   = M.fromList [("de", german), ("foo", fooLang)] :: Map Text Language
        haiku1  = "Decken auf dem Gras, eine Nacht lang ohne Haus - reich nur durch den Mond."  :: Text
        haiku2  = "a o a o a oo uu oo a o a o o a o a o" :: Text

    describe "Simple parsing" $ do
      it "Reject nonsense" $
        parseHaikuForLanguage german "Die Antwort ist 42!" `shouldBe` Nothing
      it "Correctly split Haiku" $
        parseHaikuForLanguage german haiku1
          `shouldBe` Just ( "Decken auf dem Gras,"
                          , "eine Nacht lang ohne Haus -"
                          , "reich nur durch den Mond."
                          )

    describe "Multiple languages" $ do
      it "Correctly parse a German Haiku" $
        parseHaiku langs haiku1
          `shouldBe` Just (german,  ( "Decken auf dem Gras,"
                                    , "eine Nacht lang ohne Haus -"
                                    , "reich nur durch den Mond."
                                    ))
      it "Correctly parse a foo lang Haiku" $
        parseHaiku langs haiku2
          `shouldBe` Just (fooLang, ( "a o a o a"
                                    , "oo uu oo a o a o"
                                    , "o a o a o"
                                    ))

    describe "Web service" $ do
      let input = "Decken auf dem Gras, eine Nacht lang ohne Haus - reich nur durch den Mond."
          url i = "/haiku?input=" <> encodeTextToBS i
      with (return $ hskuWebService langs) $ do
        it "Reject nonsense" $
          get (url "The answer is 42")
            `shouldRespondWith` [json|{"result": null}|]
        it "Correctly serve haiku" $
          get (url input)
            `shouldRespondWith` [json|{"result": [
                                  "Decken auf dem Gras,",
                                  "eine Nacht lang ohne Haus -",
                                  "reich nur durch den Mond."
                                  ]}|]

    describe "Command line interface" $ do
      it "Reject nonsense" $
        haikuToJSON langs "The answer is 42"
          `shouldBe` [json|{"result": null}|]
      it "Correctly return JSON haiku" $
        haikuToJSON langs haiku1
          `shouldBe`  [json|{"result": [
                        "Decken auf dem Gras,",
                        "eine Nacht lang ohne Haus -",
                        "reich nur durch den Mond."
                        ]}|]
