% Ordenacion_de_estructuras.lhs
% Ordenación de estructuras
% José A. Alonso Jiménez 
% Sevilla, 14 de mayo de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Ordenacion_de_estructuras where
\end{code}
\end{comment}

Las notas de los dos primeros exámenes se pueden representar mediante
el siguiente tipo de dato
\begin{descripcion} 
   data Notas = Notas String Int Int
          deriving (Read, Show, Eq)
\end{descripcion} 
Por ejemplo, (Notas "Juan" 6 5) representar las notas de un alumno
cuyo nombre es Juan, la nota del primer examen es 6 y la del segundo
es 5.

Definir la función
\begin{descripcion} 
   ordenadas :: [Notas] -> [Notas]
\end{descripcion} 
tal que (ordenadas ns) es la lista de las notas ns ordenadas
considerando primero la nota del examen 2, a continuación la del
examen 1 y finalmente el nombre. Por ejemplo,
\begin{descripcion} 
   ghci> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 7] 
   [Notas "Juan" 6 5,Notas "Luis" 3 7]
   ghci> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 4] 
   [Notas "Luis" 3 4,Notas "Juan" 6 5]
   ghci> ordenadas [Notas "Juan" 6 5, Notas "Luis" 7 4] 
   [Notas "Luis" 7 4,Notas "Juan" 6 5]
   ghci> ordenadas [Notas "Juan" 6 4, Notas "Luis" 7 4] 
   [Notas "Juan" 6 4,Notas "Luis" 7 4]
   ghci> ordenadas [Notas "Juan" 6 4, Notas "Luis" 5 4] 
   [Notas "Luis" 5 4,Notas "Juan" 6 4]
   ghci> ordenadas [Notas "Juan" 5 4, Notas "Luis" 5 4] 
   [Notas "Juan" 5 4,Notas "Luis" 5 4]
   ghci> ordenadas [Notas "Juan" 5 4, Notas "Eva" 5 4] 
   [Notas "Eva" 5 4,Notas "Juan" 5 4]
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List (sort)

data Notas = Notas String Int Int
  deriving (Read, Show, Eq)

ordenadas :: [Notas] -> [Notas]
ordenadas ns =
  [Notas n x y | (y,x,n) <- sort [(y1,x1,n1) | (Notas n1 x1 y1) <- ns]] 
\end{code} 
