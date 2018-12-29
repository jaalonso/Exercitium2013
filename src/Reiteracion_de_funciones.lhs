% Reiteracion_de_funciones.lhs
% Reiteración de una función.
% José A. Alonso Jiménez
% Sevilla, 10 de Mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Reiteracion_de_funciones where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   reiteracion :: Int -> (a -> a) -> a -> a
\end{descripcion} 
tal que (reiteracion n f x) es el resultado de aplicar n veces la
función f a x. Por ejemplo,
\begin{descripcion} 
   reiteracion 10 (+1) 5  ==  15
   reiteracion 10 (+5) 0  ==  50
   reiteracion  4 (*2) 1  ==  16
   reiteracion1 4 (5:) [] ==  [5,5,5,5]
\end{descripcion} 

Comprobar con QuickCheck que se verifican las siguientes propiedades
\begin{descripcion} 
   reiteracion 10 (+1) x  == 10 + x 
   reiteracion 10 (+x) 0  == 10 * x 
   reiteracion 10 (x:) [] == replicate 10 x  
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

-- 1ª solución
reiteracion :: Int -> (a -> a) -> a -> a
reiteracion 0 f x = x
reiteracion n f x = f (reiteracion (n-1) f x)

-- 2ª solución
reiteracion2 :: Int -> (a -> a) -> a -> a
reiteracion2 0 f = id
reiteracion2 n f = f . reiteracion2 (n-1) f

-- 3ª solución
reiteracion3 :: Int -> (a -> a) -> a -> a
reiteracion3 n f x = (iterate f x) !! n

-- La 1ª propiedad es
prop_suma :: Int -> Bool
prop_suma x = reiteracion 10 (+1) x  == 10 + x 

-- La 2ª propiedad es
prop_producto :: Int -> Bool
prop_producto x = reiteracion 10 (+x) 0  == 10 * x 

-- La 3ª propiedad es
prop_cons :: Int -> Bool
prop_cons x = reiteracion 10 (x:) [] == replicate 10 x  

-- Las comprobaciones son
--    ghci> quickCheck prop_suma
--    +++ OK, passed 100 tests.
--    ghci> quickCheck prop_producto
--    +++ OK, passed 100 tests.
--    ghci> quickCheck prop_cons
--    +++ OK, passed 100 tests.
\end{code} 
