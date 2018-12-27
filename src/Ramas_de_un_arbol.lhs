% Ramas_de_un_arbol.lhs
% Ramas de un árbol.
% José A. Alonso Jiménez
% Sevilla, 9 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Ejercicio propuesto el 9 de mayo de 2014}

\begin{comment}
\begin{code}
module Ramas_de_un_arbol where
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
   ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]
\end{descripcion}
 
Definir la función
\begin{descripcion} 
   ramas :: Arbol b -> [[b]]
\end{descripcion} 
tal que (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
\begin{descripcion} 
   ramas ej1  ==  [[1,2],[1,3,4]]
   ramas ej2  ==  [[3,5,6],[3,4],[3,7,2],[3,7,1]]
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

data Arbol a = N a [Arbol a]
               deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 2 [],N 3 [N 4 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- 1ª solución
ramas :: Arbol b -> [[b]]
ramas (N x []) = [[x]]
ramas (N x as) = [x : xs | a <- as, xs <- ramas a]

-- 2ª solución
ramas2 :: Arbol b -> [[b]]
ramas2 (N x []) = [[x]]
ramas2 (N x as) = concat (map (map (x:)) (map ramas2 as))

-- 3ª solución
ramas3 :: Arbol b -> [[b]]
ramas3 (N x []) = [[x]]
ramas3 (N x as) = concatMap (map (x:)) (map ramas3 as)

-- 4ª solución
ramas4 :: Arbol b -> [[b]]
ramas4 (N x []) = [[x]]
ramas4 (N x as) = concatMap (map (x :) . ramas4) as

-- 5ª solución
ramas5 :: Arbol a -> [[a]]
ramas5 (N x []) = [[x]]
ramas5 (N x xs) = map ramas5 xs >>= map (x:)

-- Equivalencia
-- ============

-- Generador de árboles.
--    > sample ((gen_Arbol 5) :: Gen (Arbol Int))
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
  arbitrary =
    sized gen_Arbol

-- La propiedad es
prop_ramas :: Arbol Int -> Bool
prop_ramas a =
  all (== ramas a)
      [f a | f <- [ ramas2
                  , ramas3
                  , ramas4
                  , ramas5
                  ]]

-- La comprobación es
--    λ> quickCheck prop_ramas
--    +++ OK, passed 100 tests.
\end{code} 
