% Iguales_al_siguiente.hs
% Iguales al siguiente.
% José A. Alonso Jiménez 
% Sevilla, 21 de Abril de 2014
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Iguales_al_siguiente where
\end{code}
\end{comment}
\note{
}

\section*{Enunciado}

Definir la función
\begin{descripcion}
   igualesAlSiguiente :: Eq a => [a] -> [a]
\end{descripcion}
tal que (igualesAlSiguiente xs) es la lista de los elementos de xs
que son iguales a su siguiente. Por ejemplo,
\begin{descripcion}
   igualesAlSiguiente [1,2,2,2,3,3,4]  ==  [2,2,3]
   igualesAlSiguiente [1..10]          ==  []
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.List (group)
import Test.QuickCheck

-- 1ª definición (por comprensión):
igualesAlSiguiente :: Eq a => [a] -> [a]
igualesAlSiguiente xs =
  [x | (x,y) <- zip xs (tail xs), x == y]

-- 2ª definición (por recursión):
igualesAlSiguiente2 :: Eq a => [a] -> [a]
igualesAlSiguiente2 (x:y:zs)
  | x == y    = x : igualesAlSiguiente2 (y:zs)
  | otherwise = igualesAlSiguiente2 (y:zs)
igualesAlSiguiente2 _ = []

-- 3ª definición (con concat y comprensión):
igualesAlSiguiente3 :: Eq a => [a] -> [a]
igualesAlSiguiente3 xs = concat [ys | (_:ys) <- group xs]

-- 4ª definición (con concat y map):
igualesAlSiguiente4 :: Eq a => [a] -> [a]
igualesAlSiguiente4 xs = concat (map tail (group xs))

-- 5ª definición (con concatMap):
igualesAlSiguiente5 :: Eq a => [a] -> [a]
igualesAlSiguiente5 xs = concatMap tail (group xs)

-- 6ª definición (con concatMap y sin argumentos):
igualesAlSiguiente6 :: Eq a => [a] -> [a]
igualesAlSiguiente6 = concatMap tail . group

-- Equivalencia
-- ============

-- La propiedad es
prop_igualesAlSiguiente_equiv :: [Int] -> Bool
prop_igualesAlSiguiente_equiv xs =
  igualesAlSiguiente xs == igualesAlSiguiente2 xs &&
  igualesAlSiguiente xs == igualesAlSiguiente3 xs &&
  igualesAlSiguiente xs == igualesAlSiguiente4 xs &&
  igualesAlSiguiente xs == igualesAlSiguiente5 xs &&
  igualesAlSiguiente xs == igualesAlSiguiente6 xs

verifica_igualesAlSiguiente_equiv :: IO ()
verifica_igualesAlSiguiente_equiv =
  quickCheck prop_igualesAlSiguiente_equiv
  
-- La comprobación es
--    λ> verifica_igualesAlSiguiente_equiv
--    +++ OK, passed 100 tests.
--    (0.07 secs, 9,911,528 bytes)
\end{code}
