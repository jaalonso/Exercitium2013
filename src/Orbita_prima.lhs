% Orbita_prima.lhs
% Órbita prima.
% José A. Alonso Jiménez
% Sevilla,  6 de Junio de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Orbita_prima where
\end{code}
\end{comment}

La órbita prima de un número n es la sucesión construida de la
siguiente forma:
\begin{itemize}
\item si n es compuesto su órbita no tiene elementos 
\item si n es primo, entonces n está en su órbita; además, sumamos n y
  sus dígitos, si el resultado es un número primo repetimos el
  proceso hasta obtener un número compuesto. 
\end{itemize}
Por ejemplo, con el 11 podemos repetir el proceso dos veces
\begin{descripcion} 
   13 = 11+1+1
   17 = 13+1+3
\end{descripcion} 
Así, la órbita prima de 11 es 11, 13, 17. 

Definir la función
\begin{descripcion} 
   orbita :: Integer -> [Integer]
\end{descripcion} 
tal que (orbita n) es la órbita prima de n. Por ejemplo,
\begin{descripcion} 
   orbita 11 == [11,13,17]
   orbita 59 == [59,73,83]
\end{descripcion} 

Calcular el menor número cuya órbita prima tiene más de 3 elementos.

\section*{Soluciones}

\begin{code} 
-- 1ª solución
-- ===========

orbita :: Integer -> [Integer]
orbita n | not (esPrimo n) = []
         | otherwise       = n : orbita (n + sum (cifras n))

esPrimo :: Integer -> Bool
esPrimo n = [x | x <- [1..n], n `rem` x == 0] == [1,n] 

cifras :: Integer -> [Integer]
cifras n = [read [x]| x <- show n]

-- 2ª solución (con iterate)
-- ===========================

orbita2 :: integer -> [integer]
orbita2 n = takewhile esprimo (iterate f n)
  where f x = x + sum (cifras x)

-- el cálculo es
--    ghci> head [x | x <- [1,3..], length (orbita x) > 3]
--    277
-- 
--    ghci> orbita 277
--    [277,293,307,317]
\end{code} 

