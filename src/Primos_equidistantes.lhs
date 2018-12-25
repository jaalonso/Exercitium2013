% Primos_equidistantes.lhs
% Primos equidistantes.
% José A. Alonso Jiménez
% Sevilla, 30 de abril de 2014
% ---------------------------------------------------------------------

\section*{Ejercicio propuesto el 30 de abril de 2014}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Primos_equidistantes where
\end{code}
\end{comment}

Definir la función
\begin{descripcion}
   primosEquidistantes :: Integer -> [(Integer,Integer)]
\end{descripcion}
tal que (primosEquidistantes k) es la lista de los pares de primos
cuya diferencia es k. Por ejemplo,
\begin{descripcion}
   take 3 (primosEquidistantes 2)  ==  [(3,5),(5,7),(11,13)]
   take 3 (primosEquidistantes 4)  ==  [(7,11),(13,17),(19,23)]
   take 3 (primosEquidistantes 6)  ==  [(23,29),(31,37),(47,53)]
   take 3 (primosEquidistantes 8)  ==  [(89,97),(359,367),(389,397)]
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.Numbers.Primes (primes)

-- 1ª solución
-- ===========

primosEquidistantes :: Integer -> [(Integer,Integer)]
primosEquidistantes k = aux primos
  where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                     | otherwise  = aux (y:ps)

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Integer -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

-- primos es la lista de los números primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
primos :: [Integer]
primos = 2 : [x | x <- [3,5..], primo x]

-- 2ª solución
-- ===========

primosEquidistantes2 :: Integer -> [(Integer,Integer)]
primosEquidistantes2 k = aux primes
  where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                     | otherwise  = aux (y:ps)

-- 3ª solución
-- ===========

primosEquidistantes3 :: Integer -> [(Integer,Integer)]
primosEquidistantes3 k =
  [(x,y) | (x,y) <- zip primes (tail primes)
         , y - x == k]

-- Comparación de eficiencia
-- =========================

--    λ> primosEquidistantes 2 !! 200
--    (9677,9679)
--    (6.30 secs, 896,925,344 bytes)
--    λ> primosEquidistantes2 2 !! 200
--    (9677,9679)
--    (0.02 secs, 3,622,808 bytes)
--    λ> primosEquidistantes3 2 !! 200
--    (9677,9679)
--    (0.03 secs, 6,398,168 bytes)
--    
--    λ> primosEquidistantes2 2 !! 20000
--    (2840447,2840449)
--    (2.46 secs, 1,142,499,552 bytes)
--    λ> primosEquidistantes3 2 !! 20000
--    (2840447,2840449)
--    (4.32 secs, 2,207,828,128 bytes)
\end{code}
