% Elimina_unitarias.lhs
% Eliminación de las ocurrencias unitarias.
% José A. Alonso Jiménez 
% Sevilla, 8 de Junio de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Elimina_unitarias where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   eliminaUnitarias :: Char -> String -> String
\end{descripcion} 
tal que (eliminaUnitarias c cs) es la lista obtenida eliminando de la
cadena cs las ocurrencias unitarias del carácter c (es decir,
aquellas ocurrencias de c tales que su elemento anterior y posterior
es distinto de c). Por ejemplo,
\begin{descripcion} 
   eliminaUnitarias 'X ""                  == ""
   eliminaUnitarias 'X "X"                 == ""
   eliminaUnitarias 'X "XX"                == "XX"
   eliminaUnitarias 'X "XXX"               == "XXX"
   eliminaUnitarias 'X "abcd"              == "abcd"
   eliminaUnitarias 'X "Xabcd"             == "abcd"
   eliminaUnitarias 'X "XXabcd"            == "XXabcd"
   eliminaUnitarias 'X "XXXabcd"           == "XXXabcd"
   eliminaUnitarias 'X "abcdX"             == "abcd"
   eliminaUnitarias 'X "abcdXX"            == "abcdXX"
   eliminaUnitarias 'X "abcdXXX"           == "abcdXXX"
   eliminaUnitarias 'X "abXcd"             == "abcd"
   eliminaUnitarias 'X "abXXcd"            == "abXXcd"
   eliminaUnitarias 'X "abXXXcd"           == "abXXXcd"
   eliminaUnitarias 'X "XabXcdX"           == "abcd"
   eliminaUnitarias 'X "XXabXXcdXX"        == "XXabXXcdXX"
   eliminaUnitarias 'X "XXXabXXXcdXXX"     == "XXXabXXXcdXXX"
   eliminaUnitarias 'X' "XabXXcdXeXXXfXx"  ==  "abXXcdeXXXfx"
\end{descripcion} 
\section*{Soluciones}

\begin{code}    
import Data.List (group)

-- 1ª solución
-- ===========

eliminaUnitarias :: Char -> String -> String
eliminaUnitarias c cs =
  concat [xs | xs <- group cs, xs /= [c]] 

-- 2ª solución
-- ===========

eliminaUnitarias2 :: Char -> String -> String
eliminaUnitarias2 c = concat . filter (/=[c]) . group

-- 3ª solución
-- ===========

eliminaUnitarias3 :: Char -> String -> String
eliminaUnitarias3 _ [] = []
eliminaUnitarias3 c [x] | c == x    = []
                        | otherwise = [x]
eliminaUnitarias3 c (x:y:zs) 
  | x /= c    = x : eliminaUnitarias3 c (y:zs)
  | y /= c    = y : eliminaUnitarias3 c zs
  | otherwise = takeWhile (==c) (x:y:zs) ++ 
                eliminaUnitarias3 c (dropWhile (==c) zs)

-- 4ª solución
-- ===========

eliminaUnitarias4 :: Char -> String -> String
eliminaUnitarias4 c cs = reverse (aux0 cs "")
  where aux0 [] cs2                  = cs2
        aux0 (x:cs1) cs2 | x == c    = aux1 cs1 cs2
                         | otherwise = aux0 cs1 (x:cs2)
        aux1 [] cs2                  = cs2
        aux1 (x:cs1) cs2 | x == c    = aux2 cs1 (c:c:cs2)
                         | otherwise = aux0 cs1 (x:cs2)
        aux2 [] cs2                  = cs2
        aux2 (x:cs1) cs2 | x == c    = aux2 cs1 (c:cs2)
                         | otherwise = aux0 cs1 (x:cs2)

-- 5ª solución
-- ===========

eliminaUnitarias5 :: Char -> String -> String
eliminaUnitarias5 c cs = 
  [x | i <- [0..length cs - 1],
       let x = cs!!i, 
       x /= c || ds!!i == c || ds!!(i+2) == c]
  where d  = if c == 'a' then 'b' else 'a'
        ds = d : cs ++ [d]

-- 6ª solución 
eliminaUnitarias6 :: Char -> String -> String
eliminaUnitarias6 _ [] = []
eliminaUnitarias6 c cs
  | ys == [c] = xs ++ eliminaUnitarias6 c zs
  | otherwise = xs ++ ys ++ eliminaUnitarias6 c zs
  where (xs,us) = span (/=c) cs
        (ys,zs) = span (==c) us
\end{code} 
