module Ordenados_por_maximo_Spec (main, spec) where

import Ordenados_por_maximo
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de ordenadosPorMaximo" $
    verifica ordenadosPorMaximo
  describe "Verificacion de ordenadosPorMaximo2" $
    verifica ordenadosPorMaximo2

verifica :: ([[Int]] -> [[Int]]) -> Spec
verifica f = do
  it "e1" $
    ordenadosPorMaximo [[3,2],[6,7,5],[1,4]] `shouldBe`
      [[3,2],[1,4],[6,7,5]]
  it "p1" $ property $
    \xs -> let ys = filter (not . null) xs
           in ordenadosPorMaximo ys == ordenadosPorMaximo2 ys
  where ordenadosPorMaximo = f
