import qualified Data.Map  as M
import qualified Data.List as L

type Asign = M.Map Char Bool

as1 :: Asign
as1 = M.fromList [ ('q', True)
                 , ('p', False)]


asigns :: [Char] -> [Asign]
asigns [] = [M.empty]
asigns (x:xs) = map (f (x,False)) as ++ map (f (x,True)) as
    where 
    f = (\(k,v) as -> M.insert k v as)
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
