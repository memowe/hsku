module Main (main) where

import Test.Hspec

import HsKu (hello)

main :: IO ()
main = hspec $ describe "HsKu tests" $ do

  context "Dummy function" $
    it "Correct greeting" $
      hello "Haskell" `shouldBe` "Hello, Haskell!"
