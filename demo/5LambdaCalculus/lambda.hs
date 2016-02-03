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