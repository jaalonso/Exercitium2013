module Alfabeto_desde_Spec (main, spec) where

import Alfabeto_desde
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de alfabetoDesde" $
    verifica alfabetoDesde
  describe "Verificacion de alfabetoDesde2" $
    verifica alfabetoDesde2
  describe "Verificacion de alfabetoDesde3" $
    verifica alfabetoDesde3
  describe "Verificacion de alfabetoDesde4" $
    verifica alfabetoDesde4
  describe "Verificacion de alfabetoDesde5" $
    verifica alfabetoDesde5
  describe "Verificacion de alfabetoDesde6" $
    verifica alfabetoDesde6
  describe "Verificacion de alfabetoDesde7" $
    verifica alfabetoDesde7

verifica :: (Char -> String) -> Spec
verifica f = do
  it "e1" $
    alfabetoDesde' 'e'  `shouldBe`  "efghijklmnopqrstuvwxyzabcd"
  it "e2" $
    alfabetoDesde' 'a'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "e3" $
    alfabetoDesde' '7'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "e4" $
    alfabetoDesde' '{'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "e5" $
    alfabetoDesde' 'B'  `shouldBe`  "abcdefghijklmnopqrstuvwxyz"
  it "p1" $ property $
    \c -> alfabetoDesde' c == alfabetoDesde c
  where alfabetoDesde' = f
