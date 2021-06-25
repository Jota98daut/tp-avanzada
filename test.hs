import qualified Data.Map  as M
import qualified Data.List as L

type Asign = M.Map Char Bool

as1 :: Asign
as1 = M.fromList [ ('q', True)
                 , ('p', False)]


-- Definición recursiva de asigns
-- Duplica la lista de asignaciones, resultando en dos listas copias
-- En una de ellas, inserta el par (x,False) a todas las asignaciones de la lista. En la otra, inserta el par (x,True)
-- De esta manera resulta una lista con todas las posibles asignaciones
asigns :: [Char] -> [Asign]
asigns [] = [M.empty]
asigns (x:xs) = map (M.insert x False) as ++ map (M.insert x True) as
    where 
    as = asigns xs

-- Funcion que dada una clave, un valor y una lista de duplas, agregue la dupla (clave,valor) a la lista
f :: (k,v) -> [(k,v)] -> [(k,v)]
f (key,val) xs = (key,val) : xs

-- Función que dada una clave, un valor y una lista de listas de duplas, agrega el par (clave,valor) a todas las listas de duplas en la lista
g :: (k,v) -> [[(k,v)]] -> [[(k,v)]]
g (k,v) xxs = map (f (k,v)) xxs

-- Función que dada una clave y una lista de listas de pares, devuelva una lista 
h k xxs = map (f (k,True)) xxs ++ map (f (k,False)) xxs

asigns' :: [Char] -> [[(Char,Bool)]]
asigns' [] = [[]]
asigns' (x:xs) = h x (asigns' xs)
