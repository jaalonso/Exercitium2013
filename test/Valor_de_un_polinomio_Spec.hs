module Valor_de_un_polinomio_Spec (main, spec) where

import Valor_de_un_polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de valor" $
    verifica valor
  describe "Verificacion de valor2" $
    verifica valor2

verifica :: (Polinomio Double -> Double -> Double) -> Spec
verifica f = do
  it "e1" $
    valor' ej_pol1 0  `shouldBe`  6
  it "e1" $
    valor' ej_pol1 1  `shouldBe`  10
  it "e1" $
    valor' ej_pol1 2  `shouldBe`  102
  it "e1" $
    valor' ej_pol2 0  `shouldBe`  6.5
  it "e1" $
    valor' ej_pol2 1  `shouldBe`  10.3
  it "e1" $
    valor' ej_pol2 3  `shouldBe`  532.7
  where valor' = f
