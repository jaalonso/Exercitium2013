% ElementosMinimales.hs
% Determinación de los elementos minimales.
% José A. Alonso Jiménez 
% Sevilla, 24 de Abril de 2014
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module ElementosMinimales where
\end{code}
\end{comment}

\section*{Enunciado}

Definir la función
\begin{descripcion}
   minimales :: Eq a => [[a]] -> [[a]]
\end{descripcion}  
tal que (minimales xss) es la lista de los elementos de xss que no
están contenidos en otros elementos de xss. Por ejemplo,
\begin{descripcion}
   minimales [[1,3],[2,3,1],[3,2,5]]        ==  [[2,3,1],[3,2,5]]
   minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  ==  [[2,3,1],[3,2,5]]
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.List (delete, nub)

minimales :: Eq a => [[a]] -> [[a]]
minimales xss = 
  [xs | xs <- xss, [ys | ys <- xss, subconjuntoPropio xs ys] == []]

-- (subconjuntoPropio xs ys) se verifica si xs es un subconjunto propio
-- de ys. Por ejemplo, 
--    subconjuntoPropio [1,3] [3,1,3]    ==  False
--    subconjuntoPropio [1,3,1] [3,1,2]  ==  True
subconjuntoPropio :: Eq a => [a] -> [a] -> Bool
subconjuntoPropio xs ys = subconjuntoPropio' (nub xs) (nub ys)
  where
    subconjuntoPropio' _  [] = False
    subconjuntoPropio' [] _  = True
    subconjuntoPropio' (x:xs') ys' = 
      x `elem` ys' && subconjuntoPropio xs' (delete x ys')  
\end{code}
