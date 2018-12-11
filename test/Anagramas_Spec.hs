module Anagramas_Spec (main, spec) where

import Anagramas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion anagramas" $
    verifica anagramas
  describe "Verificacion anagramas2" $
    verifica anagramas2
  describe "Verificacion anagramas3" $
    verifica anagramas3
  describe "Verificacion anagramas4" $
    verifica anagramas4

verifica :: (String -> [String] -> [String]) -> Spec
verifica f = do
  it "e1" $
    f "amor" ["Roma","mola","loma","moRa", "rama"] `shouldBe` ["Roma","moRa"]
  it "e2" $
    f "rama" ["aMar","amaRa","roMa","marr","aRma"] `shouldBe` ["aMar","aRma"]

