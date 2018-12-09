module Bandera_tricolor_Spec (main, spec) where

import Bandera_tricolor
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de banderaTricolor" $
    verifica banderaTricolor
  describe "Verificacion de banderaTricolor2" $
    verifica banderaTricolor2
  describe "Verificacion de banderaTricolor3" $
    verifica banderaTricolor3
  describe "Verificacion de banderaTricolor4" $
    verifica banderaTricolor4
  describe "Verificacion de banderaTricolor5" $
    verifica banderaTricolor5

verifica :: ([Color] -> [Color]) -> Spec
verifica f = do
  it "e1" $
    banderaTricolor [M,R,A,A,R,R,A,M,M]  `shouldBe`  [R,R,R,A,A,A,M,M,M]
  it "e2" $
    banderaTricolor [M,R,A,R,R,A]        `shouldBe`  [R,R,R,A,A,M]
  it "p1" $ property $
    \xs -> banderaTricolor xs == banderaTricolor2 xs
  where banderaTricolor = f
