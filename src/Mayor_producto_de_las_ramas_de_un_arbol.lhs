% Mayor_producto_de_las_ramas_de_un_arbol.lhs
% Mayor producto de las ramas de un árbol.
% José A. Alonso Jiménez
% Sevilla, 18 de Mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Mayor_producto_de_las_ramas_de_un_arbol where
\end{code}
\end{comment}

Los árboles se pueden representar mediante el siguiente tipo de datos
\begin{descripcion} 
   data Arbol a = N a [Arbol a]
                  deriving Show
\end{descripcion} 
Por ejemplo, los árboles
\begin{descripcion} 
     1               3
    / \             /|\ 
   2   3           / | \
       |          5  4  7
       4          |     /\ 
                  6    2  1
\end{descripcion} 
se representan por
\begin{descripcion} 
   ej1, ej2 :: Arbol Int
   ej1 = N 1 [N 2 [],N 3 [N 4 []]]
   ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
\end{descripcion}
 
Definir la función
\begin{descripcion} 
   mayorProducto :: (Ord a, Num a) => Arbol a -> a
\end{descripcion} 
tal que (mayorProducto a) es el mayor producto de las ramas del árbol
a. Por ejemplo,
\begin{descripcion} 
   ghci> mayorProducto (N 1 [N 2 [], N  3 []])
   3
   ghci> mayorProducto (N 1 [N 8 [], N  4 [N 3 []]])
   12
   ghci> mayorProducto (N 1 [N 2 [],N 3 [N 4 []]])
   12
   ghci> mayorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
   90
\end{descripcion} 

\section*{Soluciones}

\begin{code}
import Test.QuickCheck

data Arbol a = N a [Arbol a]
  deriving Show

-- 1ª solución
-- ===========

mayorProducto :: (Ord a, Num a) => Arbol a -> a
mayorProducto a = maximum (productosRamas a)

-- (productosRamas a) es la lista de los productos de las ramas
-- del árbol a. Por ejemplo,
--    ghci> productosRamas (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    [90,12,42,21]
productosRamas :: (Ord a, Num a) => Arbol a -> [a]
productosRamas (N x []) = [x]
productosRamas (N x xs) = [x * y | a <- xs, y <- productosRamas a]

-- 2ª solución
-- ===========

mayorProducto2 :: (Ord a, Num a) => Arbol a -> a
mayorProducto2 (N x []) = x
mayorProducto2 (N x xs)
  | x > 0     = x * maximum (map mayorProducto2 xs)
  | x == 0    = 0
  | otherwise = x * minimum (map menorProducto xs)

-- (menorProducto a) es el menor producto de las ramas del árbol
-- a. Por ejemplo,
--    ghci> menorProducto (N 1 [N 2 [], N  3 []])
--    2
--    ghci> menorProducto (N 1 [N 8 [], N  4 [N 3 []]])
--    8
--    ghci> menorProducto (N 1 [N 2 [],N 3 [N 4 []]])
--    2
--    ghci> menorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    12
menorProducto :: (Ord a, Num a) => Arbol a -> a
menorProducto (N x []) = x
menorProducto (N x xs)
  | x > 0     = x * minimum (map menorProducto xs)
  | x == 0    = 0
  | otherwise = x * maximum (map mayorProducto2 xs)

-- 3ª solución
-- ===========

mayorProducto3 :: (Ord a, Num a) => Arbol a -> a
mayorProducto3 a = maximum [product xs | xs <- ramas a]

-- (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    ghci> ramas (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    [[3,5,6],[3,4],[3,7,2],[3,7,1]]
ramas :: Arbol b -> [[b]]
ramas (N x []) = [[x]]
ramas (N x as) = [x : xs | a <- as, xs <- ramas a]

-- 4ª solución
-- ===========

mayorProducto4 :: (Ord a, Num a) => Arbol a -> a
mayorProducto4 a = maximum (map product (ramas a))

-- 5ª solución
-- ===========

mayorProducto5 :: (Ord a, Num a) => Arbol a -> a
mayorProducto5 = maximum . map product . ramas

-- Equivalencia
-- ============

-- Generador de árboles.
--    ghci> sample ((gen_Arbol 5) :: Gen (Arbol Int))
--    N 0 [N 0 []]
--    N (-2) []
--    N 4 []
--    N 2 [N 4 []]
--    N 8 []
--    N (-2) [N (-9) [],N 7 []]
--    N 11 []
--    N (-11) [N 4 [],N 14 []]
--    N 10 [N (-3) [],N 13 []]
--    N 12 [N 11 []]
--    N 20 [N (-18) [],N (-13) []]
gen_Arbol :: Arbitrary a => Int -> Gen (Arbol a)
gen_Arbol m = do
  x <- arbitrary
  n <- choose (0, m `div` 2)
  xs <- vectorOf n (gen_Arbol (m `div` 4))
  return (N x xs)

-- Incluye los árboles en Arbitrary.
instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized gen_Arbol

-- La propiedad es
prop_mayorProducto :: Arbol Int -> Bool
prop_mayorProducto a =
  all (== mayorProducto a)
      [f a | f <- [ mayorProducto2
                  , mayorProducto3
                  , mayorProducto4
                  , mayorProducto5
                  ]]

-- La comprobación es
--    ghci> quickCheck prop_mayorProducto
--    +++ OK, passed 100 tests.
\end{code} 
