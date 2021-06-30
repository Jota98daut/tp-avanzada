{-
    Trabajo practico de Programación Avanzada

    Integrantes:
    D'Autilio, Joel Victorio - 40.773.200
    Rojo, Jonathan Jair      - 43.808.360
    Rossi, Pablo Andrés      - 43.410.341
-}

import qualified Data.Map  as M

-- Definiciones de los tipos a utilizar

data Formula = VAR Char | OR Formula Formula | AND Formula Formula | EQUIV Formula Formula | NOT Formula
    deriving (Eq)

type Asign = M.Map Char Bool

-- vars devuelve la lista de proposiciones que existen 

vars' :: Formula -> [Char]
vars' (VAR c)       = [c]
vars' (OR f1 f2)    = vars' f1 ++ vars' f2
vars' (AND f1 f2)   = vars' f1 ++ vars' f2
vars' (EQUIV f1 f2) = vars' f1 ++ vars' f2
vars' (NOT f)       = vars' f

vars :: Formula -> [Char]
vars = borrarRep . vars'

-- eval, dada una formula y una asignación, dice si esa formula es verdadera o no para dicha asignación
-- {Pre: Todas las proposiciones de la formula dada ocurren en la asignacion dada}

eval :: Formula -> Asign -> Bool
eval (VAR c) as       = deMaybe $ M.lookup c as
eval (OR f1 f2) as    = eval f1 as || eval f2 as
eval (AND f1 f2) as   = eval f1 as && eval f2 as
eval (EQUIV f1 f2) as = eval f1 as == eval f2 as
eval (NOT f) as       = not (eval f as)

-- asigns: dada una lista de preposiciones, devuelve una lista con todas las asignaciones
-- Duplica la lista de asignaciones, resultando en dos listas copias
-- En una de ellas, inserta el par (x,False) a todas las asignaciones de la lista. En la otra, inserta el par (x,True)

asigns :: [Char] -> [Asign]
asigns []     = [M.empty]
asigns (x:xs) = map (M.insert x False) as ++ map (M.insert x True) as
    where as = asigns xs
                      
-- sat y val determinan si una formula es satisfacible y valida, respectivamente

sat :: Formula -> Bool      -- Alguna asignacion hace a f verdadera
sat f = or $ map (eval f) (asigns (vars f))

valid :: Formula -> Bool    -- Todas las asignaciones hacen a f verdadera
valid f = and $ map (eval f) (asigns (vars f))

{- Versiones alternativas para sat y valid
sat' :: Formula -> Bool     
sat' f = or [eval f i | i <- asigns (vars f)]

valid' :: Formula -> Bool    
valid' f = and [eval f i | i <- asigns (vars f)]
-}


-- Funciones auxiliares

-- Determina la forma en que se muestra una formula
instance Show Formula where
    show (VAR c)       = c : ""     -- Convertir c de Char a String
    show (OR f1 f2)    = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
    show (AND f1 f2)   = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
    show (EQUIV f1 f2) = show f1 ++ " = " ++ show f2
    show (NOT f1)      = "~(" ++ show f1 ++ ")"

-- Dada una lista, elimina los elementos repetidos (dejando la ultima ocurrencia)
borrarRep :: Eq a => [a] -> [a]
borrarRep [] = []
borrarRep (x:xs)
    | x `elem` xs = borrarRep xs
    | otherwise = x : borrarRep xs

-- Dado un elemento Maybe a, devuelve el valor de a
-- Se utiliza en eval ya que M.lookup devuelve Maybe Bool
deMaybe :: Maybe Bool -> Bool
deMaybe Nothing  = False
deMaybe (Just b) = b


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
esDNF (OR f1 f2) = esDNF f1 && esDNF f2
esDNF f          = esCon f


-- Variables para testeos

-- Regla dorada
golden :: Formula   -- p ^ q == p == q == p v q
golden = EQUIV (EQUIV (AND (VAR 'p') (VAR 'q')) (VAR 'p')) (EQUIV (VAR 'q') (OR (VAR 'p') (VAR 'q')))

-- Distributividad de v respecto a ^
teo1 :: Formula     -- p v (q ^ r) == (p v q) ^ (p v r)
teo1 = EQUIV (OR (VAR 'p') (AND (VAR 'q') (VAR 'r'))) (AND (OR (VAR 'p') (VAR 'q')) (OR (VAR 'p') (VAR 'r')))

-- Asociatividad de v
teo2 :: Formula     -- p v (q v r) == (p v q) v r
teo2 = EQUIV (OR (VAR 'p') (OR (VAR 'q') (VAR 'r'))) (OR (OR (VAR 'p') (VAR 'q')) (VAR 'r'))

-- De Morgan
teo3 :: Formula     -- ~(p v q) == ~p ^ ~q
teo3 = EQUIV (NOT (OR (VAR 'p') (VAR 'q'))) (AND (NOT (VAR 'p')) (NOT (VAR 'q')))

-- Ley de absorcion 
teo4 :: Formula     --(p ^ q) v q == q
teo4 = EQUIV (OR (AND (VAR 'p') (VAR 'q')) (VAR 'q')) (VAR 'q')

-- Idempotencia
teo5 :: Formula     --(p ^ p) == p
teo5 = EQUIV (AND (VAR 'p') (VAR 'p')) (VAR 'p')

-- Fórmula en dnf
dnf1 :: Formula     -- (p ^ ~q) v ~s
dnf1 = OR (AND (VAR 'p') (NOT (VAR 'q'))) (NOT (VAR 's'))

-- Fórmula no en dnf
dnf2 :: Formula     -- ~(p ^ q)
dnf2 = NOT (AND (VAR 'p') (VAR 'q'))

{- 
Testeo
*Main> valid golden
True
*Main> valid teo1
True
*Main> valid teo2
True
*Main> valid teo3
True
*Main> valid teo4
True
*Main> valid teo5
True
*Main> esDNF dnf1
True
*Main> esDNF dnf2
False
-}

-- Otras variables para demas testeos

var0 = OR (VAR 'p') (VAR 'q')
var1 = EQUIV (AND (VAR 'p') (VAR 'q')) (OR (NOT (VAR 's')) (VAR 'p'))
var2 = OR (VAR 'p') (AND (VAR 'p') (NOT (VAR 'q')))
var3 = NOT (OR (VAR 'p') (NOT (VAR 'q')))
var4 = OR (VAR 'p') (AND (NOT (AND (VAR 'p') (VAR 'q'))) (NOT (VAR 'q')))
var5 = OR (EQUIV (NOT (AND (NOT (OR (VAR 'p') (NOT (VAR 'q')))) (VAR 's'))) (NOT (AND (NOT (OR (VAR 't') (NOT (VAR 'r')))) (VAR 'g')))) (EQUIV (NOT (AND (NOT (OR (VAR 'w') (NOT (VAR 'e')))) (VAR 'y'))) (NOT (AND (NOT (OR (VAR 'k') (NOT (VAR 'd')))) (VAR 'z'))))
var6 = NOT (NOT ( VAR 'p'))

contr  = AND (VAR 'p') (NOT (VAR 'p'))
valid0 = OR (VAR 'p') (NOT (VAR 'p'))

dnf3 :: Formula   -- False
dnf3 = NOT (OR (VAR 'p') (VAR 'p'))
dnf4 :: Formula   -- True
dnf4 = OR (VAR 'p') (VAR 'p')

as1 :: Asign
as1 = M.fromList [ ('p', True)
                 , ('q', False)
                 , ('s', True)]
