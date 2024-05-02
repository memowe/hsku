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
                "name: bar\nvowels: a b c\ndiphtongs: de fg\n"
              setEnv "HSKU_LANGUAGES" dir
              loadLanguages
    it "Correct languages parsed" $
      langs `shouldBe` M.fromList [("foo", Language
        { name = "bar"
        , vowels    = S.fromList "abc"
        , diphtongs = S.fromList ["de", "fg"]})]
