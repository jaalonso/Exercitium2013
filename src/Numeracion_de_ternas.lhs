% Numeracion_de_ternas.lhs
% Numeración de las ternas de números naturales.
% José A. Alonso Jiménez
% Sevilla, 13 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Numeracion_de_ternas where
\end{code}
\end{comment}

Las ternas de números naturales se pueden ordenar como sigue
\begin{descripcion} 
   (0,0,0), 
   (0,0,1),(0,1,0),(1,0,0),
   (0,0,2),(0,1,1),(0,2,0),(1,0,1),(1,1,0),(2,0,0),
   (0,0,3),(0,1,2),(0,2,1),(0,3,0),(1,0,2),(1,1,1),(1,2,0),(2,0,1),(2,1,0),(3,0,0),
   ...
\end{descripcion} 

Definir la función
\begin{descripcion} 
   posicion :: (Int,Int,Int) -> Int
\end{descripcion} 
tal que (posicion (x,y,z)) es la posición de la terna de números
naturales (x,y,z) en la ordenación anterior. Por ejemplo,
\begin{descripcion} 
   posicion (0,1,0)  ==  2
   posicion (0,0,2)  ==  4
   posicion (0,1,1)  ==  5
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
posicion :: (Int,Int,Int) -> Int
posicion (x,y,z) = length (takeWhile (/= (x,y,z)) ternas)

-- ternas es la lista ordenada de las ternas de números naturales. Por ejemplo,
--    ghci> take 10 ternas
--    [(0,0,0),(0,0,1),(0,1,0),(1,0,0),(0,0,2),(0,1,1),(0,2,0),(1,0,1),(1,1,0),(2,0,0)]
ternas :: [(Int,Int,Int)]
ternas = [(x,y,n-x-y) | n <- [0..], x <- [0..n], y <- [0..n-x]] 
\end{code} 
