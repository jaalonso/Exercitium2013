module Iguales_al_siguiente_Spec (main, spec) where

import Iguales_al_siguiente
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de igualesAlSiguiente" $
    verifica igualesAlSiguiente
  describe "Verificacion de igualesAlSiguiente2" $
    verifica igualesAlSiguiente2
  describe "Verificacion de igualesAlSiguiente3" $
    verifica igualesAlSiguiente3
  describe "Verificacion de igualesAlSiguiente4" $
    verifica igualesAlSiguiente4
  describe "Verificacion de igualesAlSiguiente5" $
    verifica igualesAlSiguiente5
  describe "Verificacion de igualesAlSiguiente6" $
    verifica igualesAlSiguiente6

verifica :: ([Int] -> [Int]) -> Spec
verifica f = do
  it "e1" $ 
    igualesAlSiguiente [1,2,2,2,3,3,4]  `shouldBe`  [2,2,3]
  it "e2" $ 
    igualesAlSiguiente [1..10] `shouldBe`  []
  it "p1" $ property $
    \xs -> igualesAlSiguiente xs == igualesAlSiguiente6 xs 
  where igualesAlSiguiente = f
  
