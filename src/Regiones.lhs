% Regiones.lhs
% Regiones en el plano.
% José A. Alonso Jiménez
% Sevilla, 16 de Abril de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Regiones where
\end{code}
\end{comment}

En los siguientes dibujos se observa que el número máximo de regiones
en el plano generadas con 1, 2 ó 3 líneas son 2, 4 ó 7, respectivamente. 
\begin{descripcion} 
                      \  |
                       \5|
                        \|
                         \
                         |\
                         | \
               |         |  \ 
    1        1 | 3     1 | 3 \  6
   ------   ---|---   ---|----\---
    2        2 | 4     2 | 4   \ 7
               |         |      \
\end{descripcion} 

Definir la función
\begin{descripcion} 
   regiones :: Integer -> Integer  
\end{descripcion} 
tal que (regiones n) es el número máximo de regiones en el plano
generadas con n líneas. Por ejemplo,
\begin{descripcion} 
   regiones 3    ==  7  
   regiones 100  ==  5051
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

-- 1ª definición
regiones :: Integer -> Integer  
regiones 0 = 1
regiones n = regiones (n-1) + n  

-- 2ª definición
regiones2 :: Integer -> Integer
regiones2 n = n*(n+1) `div` 2 + 1

-- Equivalencia
-- ============

-- La propiedad es
prop_regiones :: Positive Integer -> Bool
prop_regiones (Positive n) =
  regiones n == regiones2 n

-- La comprobación es  
--    λ> quickCheck prop_regiones
--    +++ OK, passed 100 tests.
\end{code} 
