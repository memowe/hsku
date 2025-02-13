module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Data.Maybe
import Data.Text
import Data.Set as S
import Data.Map as M
import Text.Read (readMaybe)
import Network.URI.Encode
import System.IO
import System.IO.Temp
import System.FilePath
import System.Environment

import HsKu
import HsKu.Config
import HsKu.Load
import HsKu.JSON
import HsKu.Web

main :: IO ()
main = hspec $ describe "HsKu tests" $ do

  context "Configuration file" $ do

    describe "Read example ini file content" $ do
      let iniContent  =     "[foo]\n\nbar baz = quux quuux\nzosch= xnorfzt\n"
                        ++  "[o hai]\ncan = has cheezburgers\n"
          mini        = readMaybe iniContent :: Maybe Ini
      it "Some parsing result" $ mini `shouldSatisfy` isJust
      it "Correct values" $ fromJust mini `shouldBe` Ini (
        M.fromList  [ ("foo",   M.fromList  [ ("bar baz", "quux quuux")
                                          , ("zosch", "xnorfzt") ])
                    , ("o hai", M.fromList  [ ("can", "has cheezburgers") ])] )
    describe "Read example ini file" $ do
      ini <- runIO $ withSystemTempFile "config.ini" $ \fp h -> do
              hPutStr h "[foo]\nbar = baz\n" >> hClose h
              setEnv "HSKU_CONFIG_FILE" fp
              loadConfig
      it "Correct ini data" $
        ini `shouldBe` M.fromList [("foo", M.fromList [("bar", "baz")])]

  context "Language loading" $ do
    langs <- runIO $ withSystemTempDirectory "hsku-languages" $ \dir -> do
              writeFile (dir </> "foo.yml")
                "name: foo\nvowels: a b c\ndiphtongs: de fg\n"
              writeFile (dir </> "bar.yml")
                "name: bar\nvowels: h i j\ndiphtongs: kl mn\n"
              writeFile (dir </> "config.ini")
                ("[languages]\ndir = " ++ dir ++ "\n")
              setEnv "HSKU_CONFIG_FILE" (dir </> "config.ini")
              loadConfig >>= loadLanguages
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
      let url i = "/haiku?input=" <> encodeTextToBS i
      with (return $ hskuWebService M.empty langs) $ do
        it "Reject nonsense" $
          get (url "The answer is 42")
            `shouldRespondWith` [json|{"result": null}|]
        it "Correctly serve haiku" $
          get (url haiku1)
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
