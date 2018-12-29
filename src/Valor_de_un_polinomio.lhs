% Valor_de_un_polinomio.lhs
% Valores de polinomios representados con vectores.
% José A. Alonso Jiménez
% Sevilla, 8 de Mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Valor_de_un_polinomio where
\end{code}
\end{comment}
 
Los polinomios se pueden representar mediante vectores usando la
librería Data.Array. En primer lugar, se define el tipo de los
polinomios (con coeficientes de tipo a) mediante
\begin{descripcion} 
   type Polinomio a = Array Int a
\end{descripcion} 
Como ejemplos, definimos el polinomio
\begin{descripcion} 
   ej_pol1 :: Array Int Int
   ej_pol1 = array (0,4) [(1,2),(2,-5),(4,7),(0,6),(3,0)]
\end{descripcion} 
que representa a \(2x - 5x^2 + 7x^4 + 6\) y el polinomio
\begin{descripcion} 
   ej_pol2 :: Array Int Double
   ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]
\end{descripcion} 
que representa a \(2x - 5.2x^2 + 7x^4 + 6.5\)

Definir la función
\begin{descripcion} 
   valor :: Num a => Polinomio a -> a -> a
\end{descripcion} 
tal que (valor p b) es el valor del polinomio p en el punto b. Por
ejemplo,
\begin{descripcion} 
   valor ej_pol1 0  ==  6
   valor ej_pol1 1  ==  10
   valor ej_pol1 2  ==  102
   valor ej_pol2 0  ==  6.5
   valor ej_pol2 1  ==  10.3
   valor ej_pol2 3  ==  532.7
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.Array

type Polinomio a = Array Int a

ej_pol1, ej_pol2 :: Array Int Double
ej_pol1 = array (0,4) [(1,2),(2,-5),(4,7),(0,6),(3,0)]
ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]

-- 1ª solución
valor :: Num a => Polinomio a -> a -> a
valor p b = sum [(p!i)*b^i | i <- [0..n]]
  where (_,n) = bounds p

-- 2ª solución
valor2 :: Num a => Polinomio a -> a -> a
valor2 p b = sum [v*b^i | (i,v) <- assocs p]
\end{code} 
