% Enumera_arbol.lhs
% Enumeración de árboles binarios.
% José A. Alonso Jiménez
% Sevilla, 5 de Septiembre de 2013
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Enumera_arbol where
\end{code}
\end{comment}

Los árboles binarios se pueden representar mediante el tipo Arbol
definido por
\begin{descripcion} 
   data Arbol a = H a 
                | N a (Arbol a) (Arbol a)
                deriving Show
\end{descripcion} 
Por ejemplo, el árbol
\begin{descripcion} 
        "B"
        / \ 
       /   \
      /     \
    "B"     "A"
    / \     / \
  "A" "B" "C" "C" 
\end{descripcion} 
se puede definir por
\begin{descripcion} 
   ej1 :: Arbol String
   ej1 = N "B" (N "B" (H "A") (H "B")) (N "A" (H "C") (H "C"))
 \end{descripcion}
 
Definir la función
\begin{descripcion} 
   enumeraArbol :: Arbol t -> Arbol Int
\end{descripcion} 
tal que (enumeraArbol a) es el árbol obtenido numerando las hojas y
los nodos de a desde la hoja izquierda hasta la raíz. Por ejemplo,
\begin{descripcion} 
   ghci> enumeraArbol ej1
   N 6 (N 2 (H 0) (H 1)) (N 5 (H 3) (H 4))
\end{descripcion} 
Gráficamente,
\begin{descripcion} 
         6 
        / \ 
       /   \
      /     \
     2       5 
    / \     / \
   0   1   3   4  
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
data Arbol a = H a 
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

ej1 :: Arbol String
ej1 = N "B" (N "B" (H "A") (H "B")) (N "A" (H "C") (H "C"))

enumeraArbol :: Arbol t -> Arbol Int
enumeraArbol a = fst (aux a 0)
  where aux :: Arbol a -> Int -> (Arbol Int,Int)
        aux (H _) n     = (H n, n+1)
        aux (N x i d) n = (N n2 i' d', 1+n2)
          where (i', n1) = aux i n
                (d', n2) = aux d n1
\end{code} 
