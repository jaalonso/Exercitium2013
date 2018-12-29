% Triangulares_con_cifras.lhs
% Números triangulares con n cifras distintas.
% José A. Alonso Jiménez
% Sevilla, 8 de Mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Triangulares_con_cifras where
\end{code}
\end{comment}

Los números triangulares se forman como sigue
\begin{descripcion} 
   *     *      * 
        * *    * *
              * * *
   1     3      6
\end{descripcion} 

La sucesión de los números triangulares se obtiene sumando los
números naturales. Así, los 5 primeros números triangulares son
\begin{descripcion} 
    1 = 1
    3 = 1+2
    6 = 1+2+3
   10 = 1+2+3+4
   15 = 1+2+3+4+5
\end{descripcion}
 
Definir la función
\begin{descripcion} 
   triangularesConCifras :: Int -> [Integer]
\end{descripcion}   
tal que (triangulares n) es la lista de los números triangulares con
n cifras distintas. Por  ejemplo,
\begin{descripcion} 
   take 6 (triangularesConCifras 1)   ==  [1,3,6,55,66,666]
   take 6 (triangularesConCifras 2)   ==  [10,15,21,28,36,45]
   take 6 (triangularesConCifras 3)   ==  [105,120,136,153,190,210]
   take 5 (triangularesConCifras 4)   ==  [1035,1275,1326,1378,1485]
   take 2 (triangularesConCifras 10)  ==  [1062489753,1239845706]
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Data.List (nub)

triangularesConCifras1 :: Int -> [Integer]
triangularesConCifras1 n =
    [x | x <- triangulares, nCifras x == n]

-- 1ª definición de triangulares
-- =============================
triangulares1 :: [Integer]
triangulares1 = 1 : [x+y | (x,y) <- zip [2..] triangulares1]

-- 2ª definición de triangulares
-- =============================
triangulares2 :: [Integer]
triangulares2 = scanl (+) 1 [2..]

-- 3ª definición de triangulares
-- =============================
triangulares3 :: [Integer]
triangulares3 = [(n*(n+1)) `div` 2 | n <- [1..]]

-- Comparación de eficiencia
-- =========================

--    λ> triangulares1 !! (10^6)
--    500001500001
--    (2.03 secs, 330,448,496 bytes)
--    λ> triangulares2 !! (10^6)
--    500001500001
--    (1.09 secs, 225,631,536 bytes)
--    λ> triangulares3 !! (10^6)
--    500001500001
--    (0.92 secs, 184,405,920 bytes)

-- Usaremos como triangulares la 2ª definición:
triangulares :: [Integer]
triangulares = triangulares3

-- (nCifras x) es el número de cifras distintas del número x. Por
-- ejemplo, 
--    nCifras 325275  ==  4
nCifras :: Integer -> Int
nCifras = length . nub . show
\end{code} 
