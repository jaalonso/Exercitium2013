% Alfabeto_desde.lhs
% Alfabeto comenzando en un carácter.
% José A. Alonso Jiménez
% Sevilla, 12 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Alfabeto_desde where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   alfabetoDesde :: Char -> String
\end{descripcion} 
tal que (alfabetoDesde c) es el alfabeto, en minúscula, comenzando en
el carácter c, si c es una letra minúscula y comenzando en 'a', en
caso contrario. Por ejemplo,
\begin{descripcion} 
   alfabetoDesde 'e'  ==  "efghijklmnopqrstuvwxyzabcd"
   alfabetoDesde 'a'  ==  "abcdefghijklmnopqrstuvwxyz"
   alfabetoDesde '7'  ==  "abcdefghijklmnopqrstuvwxyz"
   alfabetoDesde '{'  ==  "abcdefghijklmnopqrstuvwxyz"
   alfabetoDesde 'B'  ==  "abcdefghijklmnopqrstuvwxyz"
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.Char (isAsciiLower, ord)
import Data.Tuple (swap)
import Test.QuickCheck

-- 1ª solución
alfabetoDesde :: Char -> String
alfabetoDesde c =
  dropWhile (<c) ['a'..'z'] ++ takeWhile (<c) ['a'..'z']

-- 2ª solución
alfabetoDesde2 :: Char -> String
alfabetoDesde2 c = ys ++ xs
  where (xs,ys) = span (<c) ['a'..'z']

-- 3ª solución
alfabetoDesde3 :: Char -> String
alfabetoDesde3 c = ys ++ xs
  where (xs,ys) = break (==c) ['a'..'z']

-- 4ª solución
alfabetoDesde4 :: Char -> String
alfabetoDesde4 = uncurry (++) . swap . flip span ['a'..'z'] . (>)

-- Ejemplo de cálculo:
--    alfabetoDesde4 'e'
--    = (uncurry (++) . swap . flip span ['a'..'z'] . (>)) 'e'
--    = (uncurry (++) . swap) ("abcd","efghijklmnopqrstuvwxyz")
--    = uncurry (++) ("efghijklmnopqrstuvwxyz","abcd")
--    = (++) "efghijklmnopqrstuvwxyz" "abcd"
--    = "efghijklmnopqrstuvwxyzabcd"

-- 5ª solución
alfabetoDesde5 :: Char -> String
alfabetoDesde5 = uncurry (flip (++)) . (`break` ['a'..'z']) . (==)

-- Ejemplo de cálculo:
--    alfabetoDesde5 'e'
--    = (uncurry (flip (++)) . (`break` ['a'..'z']) . (==)) 'e'
--    = uncurry (flip (++)) ("abcd","efghijklmnopqrstuvwxyz")
--    = flip (++) "abcd" "efghijklmnopqrstuvwxyz"
--    = "efghijklmnopqrstuvwxyz" ++ "abcd"
--    = "efghijklmnopqrstuvwxyzabcd"

-- 6ª solución
alfabetoDesde6 :: Char -> String
alfabetoDesde6 c
  | c >= 'a' && c <= 'z' = [c..'z'] ++ ['a'..pred c]
  | otherwise            = ['a'..'z']

-- 7ª solución
alfabetoDesde7 :: Char -> String
alfabetoDesde7 c
  | isAsciiLower c = [c..'z'] ++ ['a'..pred c]
  | otherwise      = ['a'..'z']


-- Equivalencia
-- ============

-- La propiedad es
prop_alfabetoDesde :: Char -> Bool
prop_alfabetoDesde c =
  all (== alfabetoDesde c)
      [f c | f <- [ alfabetoDesde2
                  , alfabetoDesde3
                  , alfabetoDesde4
                  , alfabetoDesde5
                  , alfabetoDesde6
                  , alfabetoDesde7
                  ]]

-- La comprobación es
--    λ> quickCheck prop_alfabetoDesde
--    +++ OK, passed 100 tests.
\end{code} 


  
