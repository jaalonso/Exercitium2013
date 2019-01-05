% Indices_verdaderos.lhs
% Índices de valores verdaderos.
% José A. Alonso Jiménez
% Sevilla, 21 de Abril de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Indices_verdaderos where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   indicesVerdaderos :: [Int] -> [Bool]
\end{descripcion} 
tal que (indicesVerdaderos xs) es la lista infinita de booleanos tal
que sólo son verdaderos los elementos cuyos índices pertenecen a la
lista estrictamente creciente xs. Por ejemplo,
\begin{descripcion} 
   ghci> take 6 (indicesVerdaderos [1,4])
   [False,True,False,False,True,False]
   ghci> take 6 (indicesVerdaderos [0,2..])
   [True,False,True,False,True,False]
   ghci> take 3 (indicesVerdaderos [])
   [False,False,False]
   ghci> take 6 (indicesVerdaderos [1..])
   [False,True,True,True,True,True]
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
-- 1ª solución
  
indicesVerdaderos :: [Int] -> [Bool]
indicesVerdaderos xs = [pertenece x xs | x <- [0..]]

-- (pertenece x ys) se verifica si x pertenece a la lista estrictamente
-- creciente (posiblemente infinita) ys. Por ejemplo,
--    pertenece 9 [1,3..]  ==  True
--    pertenece 6 [1,3..]  ==  False
pertenece :: Int -> [Int] -> Bool
pertenece x ys = x == head (dropWhile (<x) ys)

-- 2ª solución
-- ===========

indicesVerdaderos2 :: [Int] -> [Bool]
indicesVerdaderos2 []     = repeat False
indicesVerdaderos2 (x:ys) =
  replicate x False ++ [True] ++ indicesVerdaderos2 [y-x-1 | y <- ys]

-- 3ª solución
-- ===========

indicesVerdaderos3 :: [Int] -> [Bool]
indicesVerdaderos3 xs = aux xs 0 ++ repeat False
  where aux []     _ = []
        aux (x:xs) n | x == n    = True  : aux xs     (n+1) 
                     | otherwise = False : aux (x:xs) (n+1)

-- 4ª solución
-- ===========

indicesVerdaderos4 :: [Int] -> [Bool]
indicesVerdaderos4 xs = aux xs [0..]
  where aux (x:xs) (i:is) | i == x = True  : aux xs is
                          | x > i  = False : aux (x:xs) is
                          | x < i  = False : aux xs is
        aux _ _                    = repeat False
\end{code} 
