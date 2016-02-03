data LExp = Abs String LExp
          | App LExp LExp
          | Var String
    deriving (Show, Eq)

subst :: LExp -> String -> LExp -> LExp
subst n x orig@(Var v) | v /= x   = orig
                       | otherwise = n
subst n x (App m p) = App (subst n x m) (subst n x p)
subst n x orig@(Abs v m) | v==x  = orig
                         | otherwise = Abs v (subst n x m)


show m = m (+1) 0

numOccurs :: LExp -> LExp -> Int
numOccurs (Var u) (Var v) 
   | v==u = 1
   | otherwise = 0
numOccurs (Abs str exp) var@(Var v) = 
	numOccurs exp var
numOccurs (App exp1 exp2) var@(Var v) = 
    numOccurs exp1 var + (numOccurs exp2 var)


distictVars :: LExp -> Int
distictVars (Var str) = 0
distictVars (App exp1 exp2) = distictVars exp1 + (distictVars exp2)
distictVars (Abs str exp) = 1 + distictVars exp
