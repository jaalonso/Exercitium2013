% Trenza.lhs
% Trenzado de listas.
% José A. Alonso Jiménez 
% Sevilla, 9 de Mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Trenza where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   trenza :: [a] -> [a] -> [a]
\end{descripcion} 
tal que (trenza xs ys) es la lista obtenida intercalando los
elementos de xs e ys. Por ejemplo,
\begin{descripcion} 
   trenza [5,1] [2,7,4]             ==  [5,2,1,7]
   trenza [5,1,7] [2..]             ==  [5,2,1,3,7,4]
   trenza [2..] [5,1,7]             ==  [2,5,3,1,4,7]
   take 8 (trenza [2,4..] [1,5..])  ==  [2,1,4,5,6,9,8,13]
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
-- 1ª solución
trenza1 :: [a] -> [a] -> [a]
trenza1 xs ys = concat [[x,y] | (x,y) <- zip xs ys]

-- 2ª solución
trenza2 :: [a] -> [a] -> [a]
trenza2 xs ys = concat (zipWith par xs ys)
  where par x y = [x,y]

-- 3ª solución
trenza3 :: [a] -> [a] -> [a]
trenza3 = (concat .) . zipWith par
  where par x y = [x,y]

-- 4ª solución
trenza4 :: [a] -> [a] -> [a]
trenza4 (x:xs) (y:ys) = x : y : trenza4 xs ys
trenza4 _      _      = []
\end{code} 
