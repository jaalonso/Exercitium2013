module Ramas_de_un_arbol_Spec (main, spec) where

import Ramas_de_un_arbol
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de ramas" $
    verifica ramas
  describe "Verificacion de ramas2" $
    verifica ramas2
  describe "Verificacion de ramas3" $
    verifica ramas3
  describe "Verificacion de ramas4" $
    verifica ramas4
  describe "Verificacion de ramas5" $
    verifica ramas5

verifica :: (Arbol Int -> [[Int]]) -> Spec
verifica f = do
  it "e1" $
    ramas' ej1  `shouldBe`  [[1,2],[1,3,4]]
  it "e2" $
    ramas' ej2  `shouldBe`  [[3,5,6],[3,4],[3,7,2],[3,7,1]]
  it "p1" $ property $
    \a -> ramas' a == ramas a
  where ramas' = f
