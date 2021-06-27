import qualified Data.Map  as M
import qualified Data.List as L
import Data.Maybe

-- Definiciones de los tipos a utilizar

data Formula = VAR Char | OR Formula Formula | AND Formula Formula | EQUIV Formula Formula | NOT Formula
    deriving (Eq)

type Asign = M.Map Char Bool

-- Determina la forma en que se muestra una formula
instance Show Formula where
    show (VAR c)       = c : ""     -- Convertir c de Char a String
    show (OR f1 f2)    = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
    show (AND f1 f2)   = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
    show (EQUIV f1 f2) = show f1 ++ " = " ++ show f2
    show (NOT f1)      = "~(" ++ show f1 ++ ")"

vars' :: Formula -> [Char]
vars' (VAR c)       = [c]
vars' (OR f1 f2)    = vars' f1 ++ vars' f2
vars' (AND f1 f2)   = vars' f1 ++ vars' f2
vars' (EQUIV f1 f2) = vars' f1 ++ vars' f2
vars' (NOT f)       = vars' f

vars :: Formula ->[Char]
vars = borrarRep . vars'

eval :: Formula -> Asign -> Bool
eval (VAR c) as       = fromMaybe True $ M.lookup c as
eval (OR f1 f2) as    = eval f1 as || eval f2 as
eval (AND f1 f2) as   = eval f1 as && eval f2 as
eval (EQUIV f1 f2) as = eval f1 as == eval f2 as
eval (NOT f) as       = not (eval f as)
--eval (IMP f1 f2) as       = eval (OR (NOT f1) f2)

-- Definición recursiva de asigns
-- Duplica la lista de asignaciones, resultando en dos listas copias
-- En una de ellas, inserta el par (x,False) a todas las asignaciones de la lista. En la otra, inserta el par (x,True)
-- De esta manera resulta una lista con todas las posibles asignaciones
asigns :: [Char] -> [Asign]
asigns []     = [M.empty]
asigns (x:xs) = map (M.insert x False) as ++ map (M.insert x True) as
    where as = asigns xs

sat :: Formula -> Bool      -- Alguna asignacion hace a f verdadera
sat f = or [eval f i | i <- asigns (vars f)]

valid :: Formula -> Bool    -- Todas las asignaciones hacen a f verdadera
valid f = and [eval f i | i <- asigns (vars f)]


-- Funciones auxiliares

-- Dada una lista, elimina los elementos repetidos (dejando la ultima ocurrencia)
borrarRep :: Eq a => [a] -> [a]
borrarRep [] = []
borrarRep (x:xs)
    | x `elem` xs = borrarRep xs
    | otherwise = x : borrarRep xs


-- Seccion DNF

-- Determina si una formula dada es un literal (proposicion o negacion de una)
esLit :: Formula -> Bool
esLit (VAR c) = True
esLit (NOT f) = esLit f
esLit _       = False

-- Determina si una formula es una conjuncion o un literal
esCon :: Formula -> Bool
esCon (AND f1 f2) = esCon f1 && esCon f2
esCon f           = esLit f

-- Determina si una formula esta en forma normal disyuntiva
esDNF :: Formula -> Bool
esDNF (OR f1 f2) = esCon f1 && esCon f2
esDNF f          = esCon f


-- Variables para testeos

var0 = OR (VAR 'p') (VAR 'q')
var1 = EQUIV (AND (VAR 'p') (VAR 'q')) (OR (NOT (VAR 's')) (VAR 'p'))
var2 = OR (VAR 'p') (AND (VAR 'p') (NOT (VAR 'q')))
var3 = NOT (OR (VAR 'p') (NOT (VAR 'q')))
var4 = OR (VAR 'p') (AND (NOT (AND (VAR 'p') (VAR 'q'))) (NOT (VAR 'q')))
var5 = OR (EQUIV (NOT (AND (NOT (OR (VAR 'p') (NOT (VAR ('q'))))) (VAR 's'))) (NOT (AND (NOT (OR (VAR 't') (NOT (VAR ('r'))))) (VAR 'g')))) (EQUIV (NOT (AND (NOT (OR (VAR 'w') (NOT (VAR ('e'))))) (VAR 'y'))) (NOT (AND (NOT (OR (VAR 'k') (NOT (VAR ('d'))))) (VAR 'z'))))
var6 = NOT (NOT ( VAR 'p'))

contr  = AND (VAR 'p') (NOT (VAR 'p'))
valid0 = OR (VAR 'p') (NOT (VAR 'p'))

as1 = M.fromList [ ('p', True)
                 , ('q', False)
                 , ('s', True)]

