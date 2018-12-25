% Suma_si_todos_justos.lhs
% Suma si todos los valores son justos.
% José A. Alonso Jiménez
% Sevilla, 1 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Ejercicio propuesto el 1 de mayo de 2014}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
    
module Suma_si_todos_justos where
\end{code}
\end{comment}

Definir la función
\begin{descripcion}
   sumaSiTodosJustos :: (Num a, Eq a) => [Maybe a] -> Maybe a
\end{descripcion}  
tal que (sumaSiTodosJustos xs) es justo la suma de todos los
elementos de xs si todos son justos (es decir, si Nothing no
pertenece a xs) y Nothing en caso contrario. Por ejemplo,
\begin{descripcion}
   sumaSiTodosJustos [Just 2, Just 5]           == Just 7
   sumaSiTodosJustos [Just 2, Just 5, Nothing]  == Nothing
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.Maybe
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaSiTodosJustos :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos xs 
  | todosJustos xs = Just (sum [x | (Just x) <- xs])
  | otherwise      = Nothing
                       
-- (todosJustos xs) se verifica si todos los elementos de xs son justos
-- (es decir, si Nothing no pertenece a xs) y Nothing en caso
-- contrario. Por ejemplo,  
--    todosJustos [Just 2, Just 5]           == True
--    todosJustos [Just 2, Just 5, Nothing]  == False
todosJustos :: Eq a => [Maybe a] -> Bool
todosJustos = notElem Nothing 

-- 2ª solución
-- ===========

sumaSiTodosJustos2 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos2 xs 
  | todosJustos2 xs = Just (sum [x | (Just x) <- xs])
  | otherwise       = Nothing
                       
todosJustos2 :: Eq a => [Maybe a] -> Bool
todosJustos2 = all isJust

-- 3ª solución
-- ===========

sumaSiTodosJustos3 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos3 xs 
  | todosJustos xs = Just (sum [fromJust x | x <- xs])
  | otherwise      = Nothing

-- 4ª solución
-- ===========

sumaSiTodosJustos4 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos4 xs 
  | todosJustos xs = Just (sum (catMaybes xs))
  | otherwise      = Nothing

-- 5ª solución
-- ===========

sumaSiTodosJustos5 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos5 xs = suma (sequence xs)
  where suma Nothing   = Nothing
        suma (Just ys) = Just (sum ys)

-- Nota. En la solución anterior se usa la función  
--    sequence :: Monad m => [m a] -> m [a]
-- tal que (sequence xs) es la mónada obtenida evaluando cada una de las
-- de xs de izquierda a derecha. Por ejemplo,
--    sequence [Just 2, Just 5]   ==  Just [2,5]
--    sequence [Just 2, Nothing]  ==  Nothing
--    sequence [[2,4],[5,7]]      ==  [[2,5],[2,7],[4,5],[4,7]]
--    sequence [[2,4],[5,7],[6]]  ==  [[2,5,6],[2,7,6],[4,5,6],[4,7,6]]
--    sequence [[2,4],[5,7],[]]   ==  []

-- 6ª solución
-- ===========

sumaSiTodosJustos6 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos6 xs = fmap sum (sequence xs)

-- 7ª solución
-- ===========

sumaSiTodosJustos7 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos7 = fmap sum . sequence 

-- Equivalencia
-- ============

-- La propiedad es
prop_sumaSiTodosJustos :: [Maybe Int] -> Bool
prop_sumaSiTodosJustos xs =
  all (== sumaSiTodosJustos xs)
      [f xs | f <- [ sumaSiTodosJustos2
                   , sumaSiTodosJustos3
                   , sumaSiTodosJustos4
                   , sumaSiTodosJustos5
                   , sumaSiTodosJustos6
                   , sumaSiTodosJustos7
                   ]]

-- La comprobación es
--    λ> quickCheck prop_sumaSiTodosJustos
--    +++ OK, passed 100 tests.
\end{code}
