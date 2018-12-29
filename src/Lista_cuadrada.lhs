% Lista_cuadrada.lhs
% Lista cuadrada.
% José A. Alonso Jiménez
% Sevilla, 6 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Lista_cuadrada where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   listaCuadrada :: Int -> a -> [a] -> [[a]] 
\end{descripcion} 
tal que (listaCuadrada n x xs) es una lista de n listas de longitud n
formadas con los elementos de xs completada con x, si no xs no tiene
suficientes elementos. Por ejemplo,
\begin{descripcion} 
   listaCuadrada 3 7 [0,3,5,2,4]  ==  [[0,3,5],[2,4,7],[7,7,7]]
   listaCuadrada 3 7 [0..]        ==  [[0,1,2],[3,4,5],[6,7,8]]
   listaCuadrada 2 'p' "eva"      ==  ["ev","ap"]
   listaCuadrada 2 'p' ['a'..]    ==  ["ab","cd"]
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

-- 1ª solución
-- ===========

listaCuadrada :: Int -> a -> [a] -> [[a]] 
listaCuadrada n x xs =
  take n (grupos n (xs ++ repeat x))

-- (grupos n xs) es la lista obtenida agrupando los elementos de xs en
-- grupos de n elementos, salvo el último que puede tener menos. Por
-- ejemplo, 
--    grupos 2 [4,2,5,7,6]     ==  [[4,2],[5,7],[6]]
--    take 3 (grupos 3 [1..])  ==  [[1,2,3],[4,5,6],[7,8,9]]
grupos :: Int -> [a] -> [[a]]
grupos _ [] = []
grupos n xs = take n xs : grupos n (drop n xs)

-- 2ª solución
-- ===========

listaCuadrada2 :: Int -> a -> [a] -> [[a]]
listaCuadrada2 n x xs = 
  take n [take n (drop m xs ++ repeat x)
         | m <- [0,n..n*n]]

-- 3ª solución
-- ===========

listaCuadrada3 :: Int -> a -> [a] -> [[a]] 
listaCuadrada3 n x xs =
  take n [take n ys | ys <- iterate (drop n) (xs ++ repeat x)]

-- 4ª solución
-- ===========

listaCuadrada4 :: Int -> a -> [a] -> [[a]] 
listaCuadrada4 n x = 
  take n . map (take n) . iterate (drop n) . (++ repeat x)

-- Equivalencia
-- ============

-- La propiedad es
prop_listaCuadrada :: NonNegative Int -> Int -> [Int] -> Bool
prop_listaCuadrada (NonNegative n) x xs =
  all (== listaCuadrada n x xs)
      [f n x xs | f <- [ listaCuadrada2
                       , listaCuadrada3
                       , listaCuadrada4
                       ]]

-- La comprobación es 
--    λ> quickCheck prop_listaCuadrada
--    +++ OK, passed 100 tests.
\end{code} 
