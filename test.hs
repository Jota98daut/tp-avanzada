import qualified Data.Map as M

type Asign = M.Map Char Bool

as1 :: Asign
as1 = M.fromList [ ('q', True)
                 , ('p', False)]

