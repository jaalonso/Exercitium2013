% Segmentos_consecutivos.lhs
% Segmentos de elementos consecutivos.
% José A. Alonso Jiménez
% Sevilla, 7 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Segmentos_consecutivos where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   segmentos :: (Enum a, Eq a) => [a] -> [[a]]
\end{descripcion} 
tal que (segmentos xss) es la lista de los segmentos de xss formados
por elementos consecutivos. Por ejemplo,
\begin{descripcion} 
   segmentos [1,2,5,6,4]     ==  [[1,2],[5,6],[4]]
   segmentos [1,2,3,4,7,8,9] ==  [[1,2,3,4],[7,8,9]]
   segmentos "abbccddeeebc"  ==  ["ab","bc","cd","de","e","e","bc"]
\end{descripcion}
 
Nota: Se puede usar la función succ tal que (succ x) es el sucesor de
x. Por ejemplo,
\begin{descripcion} 
   succ 3    ==  4
   succ 'c'  ==  'd'
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

-- 1ª solución
-- ===========

segmentos :: (Enum a, Eq a) => [a] -> [[a]]
segmentos []  = []
segmentos [x] = [[x]]
segmentos (x:xs) | y == succ x = (x:y:ys):zs
                 | otherwise   = [x]:(y:ys):zs
    where ((y:ys):zs) = segmentos xs

-- 2ª solución
-- ===========

segmentos2 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos2 []  = []
segmentos2 xs = ys : segmentos2 zs
  where ys = inicial xs
        n  = length ys
        zs = drop n xs

-- (inicial xs) es el segmento inicial de xs formado por elementos
-- consecutivos. Por ejemplo,
--    inicial [1,2,5,6,4]    ==  [1,2]
--    inicial "abccddeeebc"  ==  "abc"
inicial :: (Enum a, Eq a) => [a] -> [a]
inicial [] = []
inicial (x:xs) = 
  [y | (y,_) <- takeWhile (\(u,v) -> u == v) (zip (x:xs) [x..])]
\end{code} 
