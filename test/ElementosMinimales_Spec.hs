module ElementosMinimales_Spec (main, spec) where

import ElementosMinimales
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de minimales" $
    verifica minimales

verifica :: ([[Int]] -> [[Int]]) -> Spec
verifica f = do
  it "e1" $
   minimales' [[1,3],[2,3,1],[3,2,5]]        `shouldBe`  [[2,3,1],[3,2,5]]
  it "e2" $
   minimales' [[1,3],[2,3,1],[3,2,5],[3,1]]  `shouldBe`  [[2,3,1],[3,2,5]]
  where minimales' = f
