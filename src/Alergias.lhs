% Alergias.lhs
% Código de las alergias.
% José A. Alonso Jiménez
% Sevilla, 3 de Septiembre de 2013
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Alergias where
\end{code}
\end{comment}

Para la determinación de las alergia se utiliza los siguientes
códigos para los alérgenos:
\begin{descripcion} 
   Huevos ........   1
   Cacahuetes ....   2
   Mariscos ......   4
   Fresas ........   8
   Tomates .......  16
   Chocolate .....  32
   Polen .........  64
   Gatos ......... 128
\end{descripcion} 
Así, si Juan es alérgico a los cacahuetes y al chocolate, su
puntuación es 34 (es decir, 2+32).

Los alérgenos se representan mediante el siguiente tipo de dato
\begin{descripcion} 
  data Alergeno = Huevos
                | Cacahuetes
                | Mariscos
                | Fresas
                | Tomates
                | Chocolate
                | Polen
                | Gatos
                deriving (Enum, Eq, Show, Bounded)
\end{descripcion}
              
Definir la función
\begin{descripcion} 
   alergias :: Int -> [Alergeno]
\end{descripcion} 
tal que (alergias n) es la lista de alergias correspondiente a una
puntuación n. Por ejemplo,
\begin{descripcion} 
   ghci> alergias 0
   []
   ghci> alergias 1
   [Huevos]
   ghci> alergias 2
   [Cacahuetes]
   ghci> alergias 8
   [Fresas]
   ghci> alergias 3
   [Huevos,Cacahuetes]
   ghci> alergias 5
   [Huevos,Mariscos]
   ghci> alergias 248
   [Fresas,Tomates,Chocolate,Polen,Gatos]
   ghci> alergias 255
   [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]
   ghci> alergias 509
   [Huevos,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Data.List (subsequences)

data Alergeno = Huevos
              | Cacahuetes
              | Mariscos
              | Fresas
              | Tomates
              | Chocolate
              | Polen
              | Gatos
              deriving (Enum, Eq, Show, Bounded)

-- 1ª definición (usando números binarios)
-- =======================================

alergias :: Int -> [Alergeno]
alergias n = 
  [toEnum x | (y,x) <- zip (int2bin n) [0..7], y == 1]

-- (int2bin n) es la representación binaria del número n. Por ejemplo, 
--    int2bin 10  ==  [0,1,0,1]
-- ya que 10 = 0*1 + 1*2 + 0*4 + 1*8
int2bin :: Int -> [Int]
int2bin n | n < 2     = [n]
          | otherwise = n `rem` 2 : int2bin (n `div` 2)

-- 2ª definición (sin usar números binarios)
-- =========================================

alergias2 :: Int -> [Alergeno]
alergias2 n = map toEnum (aux n 0)
  where aux 0 _                = []
        aux _ a | a > 7        = []
        aux 1 a                = [a]
        aux m a | rem m 2 == 0 = aux (div m 2) (1+a)
                | otherwise    = a : aux (div m 2) (1+a) 

-- 3ª definición (con combinaciones)
-- =================================

alergias3 :: Int -> [Alergeno]
alergias3 n = 
  [a | (a,c) <- zip alergenos codigos, c `elem` descomposicion n]

-- alergenos es la lista de los alergenos
alergenos :: [Alergeno]
alergenos = [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]

-- codigos es la lista de los códigos de los alegenos.
codigos :: [Int]
codigos = [2^x| x <- [0..7]]
    
-- (descomposicion n) es la descomposición de n (módulo 256) como sumas
-- de potencias de 2. Por ejemplo, 
--    descomposicion 3    ==  [1,2]
--    descomposicion 5    ==  [1,4]
--    descomposicion 248  ==  [8,16,32,64,128]
--    descomposicion 255  ==  [1,2,4,8,16,32,64,128]
--    descomposicion 509  ==  [1,4,8,16,32,64,128]
descomposicion :: Int -> [Int]                     
descomposicion n = 
  head [xs | xs <- subsequences codigos, sum xs == n `mod` 256]
\end{code} 
