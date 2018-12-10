module Mastermind_Spec (main, spec) where

import Mastermind
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de mastermind" $
    verifica mastermind
  describe "Verificacion de mastermind2" $
    verifica mastermind2
  describe "Verificacion de mastermind3" $
    verifica mastermind3

verifica :: ([Int] -> [Int] -> (Int,Int)) -> Spec
verifica f = do
  it "e1" $
    mastermind [2,6,0,7] [1,4,0,6]  `shouldBe`  (1,1)
  it "e2" $
    mastermind [2,6,0,7] [3,5,9,1]  `shouldBe`  (0,0)
  it "e3" $
    mastermind [2,6,0,7] [1,6,0,4]  `shouldBe`  (2,0)
  it "e4" $
    mastermind [2,6,0,7] [2,6,0,7]  `shouldBe`  (4,0)
  where mastermind = f
