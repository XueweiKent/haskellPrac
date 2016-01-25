--1. 
sqr a = a * a
-- sqr :: Num a => a -> a.  it means "a" has to be a Num

--2. pass function as parameter
compose f g x = f (g x)
--compose :: (t1->t) -> (t2->t1) -> t2 -> t

--3. Lambda form
--(\x -> x + 1) 41
--42
plus = \a -> \b -> a + b

--4. map
map' f [] = []
map' f (x:xs) = f x : map' f xs

--5. foldr
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)
--sumlist = foldr (+) 0

