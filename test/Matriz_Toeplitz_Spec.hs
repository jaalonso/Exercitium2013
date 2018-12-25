module Matriz_Toeplitz_Spec (main, spec) where

import Matriz_Toeplitz
import Data.Array
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de esToeplitz" $
    verifica esToeplitz

verifica :: (Array (Int,Int) Int -> Bool) -> Spec
verifica f = do
  it "e1" $
    esToeplitz' ej1  `shouldBe`  True
  it "e2" $
    esToeplitz' ej2  `shouldBe`  False
  where esToeplitz' = f
  
