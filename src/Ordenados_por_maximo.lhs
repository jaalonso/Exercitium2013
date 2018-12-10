% Ordenados_por_maximo.hs
% Ordenación por el máximo.
% José A. Alonso Jiménez 
% Sevilla, 22 de Abril de 2014
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Ordenados_por_maximo where
\end{code}
\end{comment}

\section*{Ejercicio propuesto el 22 de Abril de 2014}

Definir la función
\begin{descripcion}
   ordenadosPorMaximo :: Ord a => [[a]] -> [[a]]
\end{descripcion}
tal que (ordenadosPorMaximo xss) es la lista de los elementos de xss
ordenada por sus máximos. Por ejemplo,
\begin{descripcion}
   ghci> ordenadosPorMaximo [[3,2],[6,7,5],[1,4]]
   [[3,2],[1,4],[6,7,5]]
   ghci> ordenadosPorMaximo ["este","es","el","primero"]
   ["el","primero","es","este"]
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.List (sort)
import GHC.Exts  (sortWith)
import Test.QuickCheck

-- 1ª definición
ordenadosPorMaximo :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo xss =
  map snd (sort [(maximum xs,xs) | xs <- xss])

-- 2ª definición
ordenadosPorMaximo2 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo2 xss =
  [xs | (_,xs) <- sort [(maximum xs,xs) | xs <- xss]]

-- 3ª definición:
ordenadosPorMaximo3 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo3 = sortWith maximum

-- Equivalencia
-- ============

verificaOrdenadosPorMaximo :: IO ()
verificaOrdenadosPorMaximo = 
  quickCheck prop_ordenadosPorMaximo

prop_ordenadosPorMaximo :: [[Int]] -> Bool
prop_ordenadosPorMaximo xs =
  ordenadosPorMaximo ys == ordenadosPorMaximo2 ys
  where ys = filter (not . null) xs

-- Comprobación
--    λ> verificaOrdenadosPorMaximo
--    +++ OK, passed 100 tests.
\end{code}
