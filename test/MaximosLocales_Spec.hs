module MaximosLocales_Spec (main, spec) where

import MaximosLocales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de maximosLocales" $
    verifica maximosLocales
  describe "Verificacion de maximosLocales2" $
    verifica maximosLocales2

verifica :: ([Int] -> [Int]) -> Spec
verifica f = do
  it "e1" $
    maximosLocales' [3,2,5,3,7,7,1,6,2]  `shouldBe`  [5,6]
  it "e2" $
    maximosLocales' [1..100]             `shouldBe`  []
  it "p1" $ property 
    prop_maximosLocales
  where maximosLocales' = f




  
