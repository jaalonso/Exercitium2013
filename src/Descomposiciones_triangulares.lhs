% Descomposición_triangular.lhs
% Descomposiciones triangulares.
% José A. Alonso Jiménez 
% Sevilla,  4 de Junio de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Descomposición_triangular where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   descomposicionesTriangulares :: Int -> [(Int, Int, Int)] 
\end{descripcion} 
tal que (descomposicionesTriangulares n) es la lista de las
ternas correspondientes a las descomposiciones de n en tres sumandos,
como máximo, formados por números triangulares. Por ejemplo,
\begin{descripcion} 
   ghci> descomposicionesTriangulares 6
   [(0,0,6),(0,3,3)]
   ghci> descomposicionesTriangulares 26
   [(1,10,15),(6,10,10)]
   ghci> descomposicionesTriangulares 96
   [(3,15,78),(6,45,45),(15,15,66),(15,36,45)]
 \end{descripcion}

\section*{Soluciones}

\begin{code}  
descomposicionesTriangulares :: Int -> [(Int, Int, Int)] 
descomposicionesTriangulares n =         
  [(x,y,n-x-y) | x <- xs, 
                 y <- dropWhile (<x) xs, 
                 n-x-y `elem` dropWhile (<y) xs]
  where xs = takeWhile (<=n) triangulares

-- triangulares es la lista de los números triangulares. Por ejemplo,
--    take 10 triangulares  ==  [0,1,3,6,10,15,21,28,36,45]
triangulares :: [Int]
triangulares = scanl (+) 0 [1..]
\end{code} 
