--neglist
negList xx = map (*(-1)) xx

--sumSqr
sumSqr xx = foldr (\a b -> (a*a+b) ) 0 xx

--map foldr
--map f xx = foldr ( \a b -> f a : b ) 0 xx

--zipWith
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : (zipWith' f xs ys)