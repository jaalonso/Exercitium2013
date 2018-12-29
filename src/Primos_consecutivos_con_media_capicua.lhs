% Primos_consecutivos_con_media_capicua.hs
% Primos consecutivos con media capicúa.
% José A. Alonso Jiménez 
% Sevilla, 28 de Abril de 2014
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Primos_consecutivos_con_media_capicua where
\end{code}
\end{comment}

\section*{Enunciado}

La pasada semana, Antonio Roldán publicó en
\href{http://bit.ly/1mPSepM}{Twitter} la siguiente observación:
\begin{quotation}
   Los pares de primos consecutivos (97,101) y (109,113) son los más
   pequeños, con promedio capicúa con más de una cifra: (97+101)/2=99
   y (109+113)/2=111.  
\end{quotation}
A partir de ella, se propone el ejercicio de hoy.

Definir la constante
\begin{descripcion}
   primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
\end{descripcion}
tal que primosConsecutivosConMediaCapicua es la lista de las ternas 
(x,y,z) tales que x e y son primos consecutivos tales que su media,
z, es capicúa. Por ejemplo,
\begin{descripcion}
   ghci> take 5 primosConsecutivosConMediaCapicua
   [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
\end{descripcion}   
Calcular cuántos hay anteriores a 2014.

\section*{Soluciones}

\begin{code}
import Data.Numbers.Primes (primes)

primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
primosConsecutivosConMediaCapicua =
  [(x,y,z) | (x,y) <- zip (tail primes) (tail (tail primes))
           , let z = (x + y) `div` 2
           , capicua z]

-- (capicua x) se verifica si x es capicúa. Por ejemplo,
capicua :: Int -> Bool
capicua x = ys == reverse ys
  where ys = show x

-- El cálculo es
-- λ> length (takeWhile (\(_,y,_) -> y < 2014) primosConsecutivosConMediaCapicua)
--  20
\end{code}
