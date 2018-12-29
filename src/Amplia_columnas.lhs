% Amplia_columnas.hs
% Ampliación de columnas de una matriz
% José A. Alonso Jiménez
% Sevilla, 16 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Amplia_columnas where
\end{code}
\end{comment}

Las matrices enteras se pueden representar mediante tablas con
índices enteros:
\begin{descripcion} 
   type Matriz = Array (Int,Int) Int
\end{descripcion}
 
Definir la función
\begin{descripcion} 
   ampliaColumnas :: Matriz -> Matriz -> Matriz
\end{descripcion} 
tal que (ampliaColumnas p q) es la matriz construida añadiendo las
columnas de la matriz q a continuación de las de p (se supone que
tienen el mismo número de filas). Por ejemplo, si p y q representa
las dos primeras matrices, entonces (ampliaColumnas p q) es la
tercera
\begin{descripcion} 
   |0 1|    |4 5 6|    |0 1 4 5 6| 
   |2 3|    |7 8 9|    |2 3 7 8 9|
\end{descripcion}    
En Haskell,
\begin{descripcion} 
   ghci> :{
   *Main| ampliaColumnas (listArray ((1,1),(2,2)) [0..3]) 
   *Main|                (listArray ((1,1),(2,3)) [4..9])
   *Main| :}
   array ((1,1),(2,5)) 
         [((1,1),0),((1,2),1),((1,3),4),((1,4),5),((1,5),6),
          ((2,1),2),((2,2),3),((2,3),7),((2,4),8),((2,5),9)]
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.Array

type Matriz = Array (Int,Int) Int

ampliaColumnas :: Matriz -> Matriz -> Matriz
ampliaColumnas p1 p2 =
  array ((1,1),(m,n1+n2)) [((i,j), f i j) | i <- [1..m], j <- [1..n1+n2]]
  where ((_,_),(m,n1)) = bounds p1
        ((_,_),(_,n2)) = bounds p2
        f i j | j <= n1   = p1!(i,j)
              | otherwise = p2!(i,j-n1) 
\end{code} 
