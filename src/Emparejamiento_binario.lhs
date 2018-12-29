% Emparejamiento_binario.lhs
% Emparejamiento binario.
% José A. Alonso Jiménez
% Sevilla, 15 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Emparejamiento_binario where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   zipBinario :: [a -> b -> c] -> [a] -> [b] -> [c]
\end{descripcion}   
tal que (zipBinario fs xs ys) es la lista obtenida aplicando cada una
de las operaciones binarias de fs a los correspondientes elementos de
xs e ys. Por ejemplo,
\begin{descripcion} 
   zipBinario [(+), (*), (*)] [2,2,2] [4,4,4]     ==  [6,8,8]
   zipBinario [(+)] [2,2,2] [4,4,4]               ==  [6]
   zipBinario (cycle [(+), (*)]) [1 .. 4] [2..5]  ==  [3,6,7,20]
\end{descripcion}

\section*{Soluciones}

\begin{code}  
-- 1ª definición
zipBinario :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario (f:fs) (x:xs) (y:ys) = f x y : zipBinario fs xs ys
zipBinario _ _ _                = []

-- 2ª definición
zipBinario2 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario2 fs xs ys = [f x y | (f,(x,y)) <- zip fs (zip xs ys)]

-- 3ª definición
zipBinario3 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario3 fs xs ys = [f x y | (f,x,y) <- zip3 fs xs ys]

-- 4ª definición
zipBinario4 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario4 = zipWith3 id 
\end{code} 
