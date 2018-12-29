% Biparticiones_de_una_lista.khs
% Biparticiones de una lista.
% José A. Alonso Jiménez
% Sevilla, 19 de Mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Biparticiones_de_una_lista where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   biparticiones :: [a] -> [([a],[a])]
\end{descripcion} 
tal que (biparticiones xs) es la lista de pares formados por un
prefijo de xs y el resto de xs. Por ejemplo,
\begin{descripcion} 
   ghci> biparticiones [3,2,5]
   [([],[3,2,5]),([3],[2,5]),([3,2],[5]),([3,2,5],[])]
   ghci> biparticiones "Roma"
   [("","Roma"),("R","oma"),("Ro","ma"),("Rom","a"),("Roma","")]
\end{descripcion}

\section*{Soluciones}

\begin{code}  
import Data.List (inits, tails)
import Control.Applicative (liftA2)

-- 1ª solución
biparticiones1 :: [a] -> [([a],[a])]
biparticiones1 xs = [splitAt i xs | i <- [0..length xs]]

-- 2ª solución
biparticiones2 :: [a] -> [([a],[a])]
biparticiones2 xs = zip (inits xs) (tails xs)

-- 3ª solución
biparticiones3 :: [a] -> [([a],[a])]
biparticiones3 [] = [([],[])] 
biparticiones3 (x:xs) = 
  ([],(x:xs)) : [(x:ys,zs) | (ys,zs) <- biparticiones3 xs]

-- 4ª solución
biparticiones4 :: [a] -> [([a],[a])]
biparticiones4 = liftA2 zip inits tails

-- 5ª solución (sin argumentos):
biparticiones5 :: [a] -> [([a],[a])]
biparticiones5 = zip <$> inits <*> tails
\end{code} 
