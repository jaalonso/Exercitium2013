% EmparejamientoDeArboles.lhs
% Emparejamiento de árboles.
% José A. Alonso Jiménez
% Sevilla, 1 de Junio de 2014
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module EmparejamientoDeArboles where
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
   6   3           / | \
       |          5  4  7
       5          |     /\ 
                  6    2  1
\end{descripcion} 
se representan por
\begin{descripcion} 
   ej1, ej2 :: Arbol Int
   ej1 = N 1 [N 6 [],N 3 [N 5 []]]
   ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
\end{descripcion}
 
Definir la función
\begin{descripcion} 
   emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
\end{descripcion} 
tal que (emparejaArboles f a1 a2) es el árbol obtenido aplicando la
función f a los elementos de los árboles a1 y a2 que se encuentran en
la misma posición. Por ejemplo,
\begin{descripcion} 
   ghci> emparejaArboles (+) (N 1 [N 2 [], N 3[]]) (N 1 [N 6 []])
   N 2 [N 8 []]
   ghci> emparejaArboles (+) ej1 ej2
   N 4 [N 11 [],N 7 []]
   ghci> emparejaArboles (+) ej1 ej1
   N 2 [N 12 [],N 6 [N 10 []]]
\end{descripcion}
 
\section*{Soluciones}

\begin{code}    
data Arbol a = N a [Arbol a]
  deriving (Show, Eq)

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 6 [],N 3 [N 5 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

emparejaArboles1 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles1 f (N x l1) (N y l2) = 
  N (f x y) (zipWith (emparejaArboles1 f) l1 l2)
\end{code} 
