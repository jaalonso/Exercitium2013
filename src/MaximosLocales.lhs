% MaximosLocales.lhs
% Máximos locales.
% José A. Alonso Jiménez
% Sevilla, 5 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
    
module MaximosLocales where
\end{code}
\end{comment}

Un máximo local de una lista es un elemento de la lista que es mayor
que su predecesor y que su sucesor en la lista. Por ejemplo, 5 es un
máximo local de [3,2,5,3,7,7,1,6,2] ya que es mayor que 2 (su
predecesor) y que 3 (su sucesor).

Definir la función
\begin{descripcion}
   maximosLocales :: Ord a => [a] -> [a]
\end{descripcion} 
tal que (maximosLocales xs) es la lista de los máximos locales de la
lista xs. Por ejemplo,
\begin{descripcion} 
   maximosLocales [3,2,5,3,7,7,1,6,2]  ==  [5,6]
   maximosLocales [1..100]             ==  []
   maximosLocales "adbpmqexyz"         ==  "dpq"
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Test.QuickCheck

-- 1ª solución
maximosLocales :: Ord a => [a] -> [a]
maximosLocales (x:y:z:xs) | y > x && y > z = y : maximosLocales (z:xs)
                          | otherwise      = maximosLocales (y:z:xs)
maximosLocales _                           = []

-- 2ª solución
maximosLocales2 :: Ord a => [a] -> [a]
maximosLocales2 xs = 
  [y | (x,y,z) <- zip3 xs (tail xs) (drop 2 xs), y > x, y > z]

-- Equivalencia
-- ============

-- La propiedad es
prop_maximosLocales :: [Int] -> Bool
prop_maximosLocales xs =
  maximosLocales xs == maximosLocales2 xs

-- La comprobación es
--    λ> quickCheck prop_maximosLocales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> maximosLocales [1..10^6]
--    []
--    (2.50 secs, 464,396,256 bytes)
--    λ> maximosLocales2 [1..10^6]
--    []
--    (1.36 secs, 280,395,256 bytes)
--    
--    λ> maximosLocales (concat (replicate 10 [1..10^5]))
--    [100000,100000,100000,100000,100000,100000,100000,100000,100000]
--    (2.55 secs, 455,647,560 bytes)
--    λ> maximosLocales2 (concat (replicate 10 [1..10^5]))
--    [100000,100000,100000,100000,100000,100000,100000,100000,100000]
--    (1.34 secs, 271,650,096 bytes)
\end{code}
