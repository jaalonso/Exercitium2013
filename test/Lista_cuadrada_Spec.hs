module Lista_cuadrada_Spec (main, spec) where

import Lista_cuadrada
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de listaCuadrada" $
    verifica listaCuadrada
  describe "Verificacion de listaCuadrada2" $
    verifica listaCuadrada2
  describe "Verificacion de listaCuadrada3" $
    verifica listaCuadrada3
  describe "Verificacion de listaCuadrada4" $
    verifica listaCuadrada4

verifica :: (Int -> Int -> [Int] -> [[Int]]) -> Spec
verifica f = do
  it "e1" $
    listaCuadrada' 3 7 [0,3,5,2,4]  `shouldBe`  [[0,3,5],[2,4,7],[7,7,7]]
  it "e2" $
    listaCuadrada' 3 7 [0..]        `shouldBe`  [[0,1,2],[3,4,5],[6,7,8]]
  it "p1" $ property $
   prop_equiv f
  where listaCuadrada' = f

prop_equiv :: (Int -> Int -> [Int] -> [[Int]])
              -> NonNegative Int -> Int -> [Int] -> Bool
prop_equiv f (NonNegative n) x xs =
  f n x xs == listaCuadrada n x xs
