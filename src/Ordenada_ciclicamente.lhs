% Ordenada_ciclicamente.lhs
% Ordenada cíclicamente.
% José A. Alonso Jiménez 
% Sevilla, 7 de Junio de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Ordenada_ciclicamente where
\end{code}
\end{comment}

Se dice que una sucesión x(1), ..., x(n) está ordenada cíclicamente
si existe un índice i tal que la sucesión
\begin{descripcion} 
   x(i), x(i+1), ..., x(n), x(1), ..., x(i-1)
\end{descripcion} 
está ordenada crecientemente. 

Definir la función
\begin{descripcion} 
   ordenadaCiclicamente :: Ord a => [a] -> Int
\end{descripcion} 
tal que (ordenadaCiclicamente xs) es el índice (empezando en 1) a
partir del cual está ordenada, si el la lista está ordenado cíclicamente
y 0 en caso contrario. Por ejemplo,
\begin{descripcion} 
   ordenadaCiclicamente [1,2,3,4]      ==  1  
   ordenadaCiclicamente [5,8,2,3]      ==  3 
   ordenadaCiclicamente [4,6,7,5,4,3]  ==  0 
   ordenadaCiclicamente [1,0,1,2]      ==  0
   ordenadaCiclicamente [0,2,0]        ==  3
   ordenadaCiclicamente "cdeab"        ==  4
\end{descripcion}

\section*{Soluciones}

\begin{code} 
-- 1ª solución
-- ===========

ordenadaCiclicamente :: Ord a => [a] -> Int
ordenadaCiclicamente xs = 
  primero [n+1 | n <- [0..length xs-1], 
                 ordenada (drop n xs ++ take n xs)]
  where primero []    = 0
        primero (x:_) = x

-- (ordenada xs) se verifica si la lista xs está ordenada
-- crecientemente. Por ejemplo,
--   ordenada "acd"   ==  True
--   ordenada "acdb"  ==  False
ordenada :: Ord a => [a] -> Bool 
ordenada (x:y:zs) = x <= y && ordenada (y:zs) 
ordenada _        = True 
 
-- 2ª solución
-- ===========

ordenadaCiclicamente2 :: Ord a => [a] -> Int
ordenadaCiclicamente2 xs = aux xs 1 (length xs)  
  where aux xs' i k 
          | i > k        = 0 
          | ordenada xs' = i 
          | otherwise    = aux (siguienteCiclo xs') (i+1) k 

-- (siguienteCiclo xs) es la lista obtenida añadiendo el primer elemento
-- de xs al final del resto de xs. Por ejemplo,
--   siguienteCiclo [3,2,5]  ==  [2,5,3]
siguienteCiclo :: [a] -> [a]
siguienteCiclo [] = [] 
siguienteCiclo (x:xs) = xs ++ [x] 
\end{code}
