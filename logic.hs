import qualified Data.Map as M
import Data.Maybe

data Formula = VAR Char | OR Formula Formula | AND Formula Formula | EQUIV Formula Formula | NOT Formula
    deriving (Eq)

type Asign = M.Map Char Bool

instance Show Formula where
    show (VAR c) = show c
    show (OR f1 f2) = show f1 ++ " v " ++ show f2
    show (AND f1 f2) = show f1 ++ " ^ " ++ show f2
    show (EQUIV f1 f2) = show f1 ++ " = " ++ show f2
    show (NOT f1) = "~ (" ++ show f1 ++ ")"

var0 = OR (VAR 'p') (VAR 'q')
var1 = EQUIV (AND (VAR 'p') (VAR 'q')) (OR (NOT (VAR 's')) (VAR 'p'))
var2 = OR (VAR 'p') (AND (VAR 'p') (NOT (VAR 'q')))
var3 = NOT (OR (VAR 'p') (NOT (VAR 'q')))
var4 = OR (VAR 'p') (AND (NOT (AND (VAR 'p') (VAR 'q'))) (NOT (VAR 'q')))

as1 = M.fromList [ ('p', True)
                 , ('q', False)
                 , ('s', True)]


vars :: Formula -> [Char]
vars (VAR c) = [c]
vars (OR f1 f2) = vars f1 ++ vars f2
vars (AND f1 f2) = vars f1 ++ vars f2
vars (EQUIV f1 f2) = vars f1 ++ vars f2
vars (NOT f) = vars f

eval :: Formula -> Asign -> Bool
eval (VAR c) as       = fromMaybe True $ M.lookup c as
eval (OR f1 f2) as    = (eval f1 as) || (eval f2 as)
eval (AND f1 f2) as   = (eval f1 as) && (eval f2 as)
eval (EQUIV f1 f2) as = (eval f1 as) == (eval f2 as)
eval (NOT f) as       = not (eval f as)

esLit :: Formula -> Bool
esLit (VAR c) = True
esLit (NOT f) = esLit f
esLit _ = False

esCon :: Formula -> Bool
esCon (AND f1 f2) = esCon f1 && esCon f2
esCon f = esLit f

esDNF :: Formula -> Bool
esDNF (OR f1 f2) = esCon f1 && esCon f2
esDNF f = esCon f

