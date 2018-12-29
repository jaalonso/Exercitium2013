% Algun_vecino_menor.lhs
% Algún vecino menor.
% José A. Alonso Jiménez
% Sevilla, 29 de diciembre de 2018
% =============================================================================

\section*{Enunciado}

\begin{comment}
\begin{code}
module Algun_vecino_menor where
\end{code}
\end{comment}

Las matrices puede representarse mediante tablas cuyos
índices son pares de números naturales:
\begin{descripcion} 
   type Matriz = Array (Int,Int) Int
\end{descripcion} 

Definir la función
\begin{descripcion} 
   algunMenor :: Matriz -> [Int]
\end{descripcion} 
tal que (algunMenor p) es la lista de los elementos de p que tienen
algún vecino menor que él. Por ejemplo,
\begin{descripcion} 
   ghci> algunMenor (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,4,2,5,4])
   [9,4,6,5,8,7,4,2,5,4]
\end{descripcion} 
pues sólo el 1 y el 3 no tienen ningún vecino menor en la matriz
\begin{descripcion} 
   |9 4 6 5|
   |8 1 7 3|
   |4 2 5 4|
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Data.Array

type Matriz = Array (Int,Int) Int

algunMenor :: Matriz -> [Int]
algunMenor p = 
  [p!(i,j) | (i,j) <- indices p,
             or [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]] 
  where (_,(m,n)) = bounds p
        vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                 b <- [max 1 (j-1)..min n (j+1)],
                                 (a,b) /= (i,j)]
\end{code} 
