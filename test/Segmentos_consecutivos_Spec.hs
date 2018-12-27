module Segmentos_consecutivos_Spec (main, spec) where

import Segmentos_consecutivos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de segmentos" $
    verifica segmentos
  describe "Verificacion de segmentos2" $
    verifica segmentos2

verifica :: ([Int] -> [[Int]]) -> Spec
verifica f = do
  it "e1" $
    segmentos' [1,2,5,6,4]     `shouldBe`  [[1,2],[5,6],[4]]
  it "e2" $
    segmentos' [1,2,3,4,7,8,9] `shouldBe`  [[1,2,3,4],[7,8,9]]
  it "p1" $ property $
    \xs -> segmentos' xs == segmentos xs
  where segmentos' = f
  
