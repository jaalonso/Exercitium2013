% Matriz_Toeplitz.lhs
% Matrices de Toepliz
% José A. Alonso Jiménez
% Sevilla, 2 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Matriz_Toeplitz where
\end{code}
\end{comment}

Una matriz de Toeplitz es una matriz cuadrada que es constante a lo
largo de las diagonales paralelas a la diagonal principal. Por ejemplo,
\begin{descripcion}
   |2 5 1 6|       |2 5 1 6|          
   |4 2 5 1|       |4 2 6 1|
   |7 4 2 5|       |7 4 2 5|
   |9 7 4 2|       |9 7 4 2|
\end{descripcion}
la primera es una matriz de Toeplitz y la segunda no lo es. 

Las anteriores matrices se pueden definir por
\begin{descripcion}
   ej1, ej2 :: Array (Int,Int) Int
   ej1 = listArray ((1,1),(4,4)) [2,5,1,6,4,2,5,1,7,4,2,5,9,7,4,2]
   ej2 = listArray ((1,1),(4,4)) [2,5,1,6,4,2,6,1,7,4,2,5,9,7,4,2]
\end{descripcion}
 
Definir la función
\begin{descripcion}
   esToeplitz :: Eq a => Array (Int,Int) a -> Bool
\end{descripcion}
tal que (esToeplitz p) se verifica si la matriz p es de Toeplitz. Por
ejemplo,
\begin{descripcion}
   esToeplitz ej1  ==  True
   esToeplitz ej2  ==  False
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.Array

ej1, ej2 :: Array (Int,Int) Int
ej1 = listArray ((1,1),(4,4)) [2,5,1,6,4,2,5,1,7,4,2,5,9,7,4,2]
ej2 = listArray ((1,1),(4,4)) [2,5,1,6,4,2,6,1,7,4,2,5,9,7,4,2]

esToeplitz :: Eq a => Array (Int,Int) a -> Bool
esToeplitz p = m == n && 
               and [ p!(i,j) == p!(i+1,j+1)
                   | i <- [1..n-1]
                   , j <- [1..n-1]]
  where (_,(m,n)) = bounds p
\end{code}
