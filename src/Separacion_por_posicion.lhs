% Separacion_por_posicion.lhs
% Separación por posición.
% José A. Alonso Jiménez
% Sevilla, 2 de Junio de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Separacion_por_posicion where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   particion :: [a] -> ([a],[a])
\end{descripcion} 
tal que (particion xs) es el par cuya primera componente son los
elementos de xs en posiciones pares y su segunda componente son los
restantes elementos. Por ejemplo,
\begin{descripcion} 
   particion [3,5,6,2]    ==  ([3,6],[5,2])
   particion [3,5,6,2,7]  ==  ([3,6,7],[5,2])
   particion "particion"  ==  ("priin","atco")
\end{descripcion}

\section*{Soluciones}

\begin{code} 
-- 1ª solución
-- ===========

particion :: [a] -> ([a],[a])
particion xs = (pares xs, impares xs)

-- (pares xs) es la lista de los elementos de xs en posiciones
-- pares. Por ejemplo,
--    pares [3,5,6,2]  ==  [3,6]
pares :: [a] -> [a]
pares []     = []
pares (x:xs) = x : impares xs

-- (impares xs) es la lista de los elementos de xs en posiciones
-- impares. Por ejemplo,
--    impares [3,5,6,2]  ==  [5,2]
impares :: [a] -> [a]
impares []     = []
impares (_:xs) = pares xs

-- 2ª solución
-- ===========

particion2 :: [a] -> ([a],[a])
particion2 xs = ([x | (x,y) <- ps, even y], [x | (x,y) <- ps, odd y])
  where ps = zip xs [0..]

-- 3ª solución
-- ===========

particion3 :: [a] -> ([a],[a])
particion3 xs = 
  ([xs!!k | k <- [0,2..n]],[xs!!k | k <- [1,3..n]]) 
  where n = length xs - 1

-- 4ª solución
-- ===========

particion4 :: [a] -> ([a],[a])
particion4 []     = ([],[])
particion4 (x:xs) = (x:zs,ys)
  where (ys,zs) = particion4 xs  

-- 5ª solución
-- ===========

particion5 :: [a] -> ([a],[a])
particion5 = foldr f ([],[])
  where f x (ys,zs) = (x:zs,ys)
\end{code} 
