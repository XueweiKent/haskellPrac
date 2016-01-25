--1. 
sqr a = a * a
-- sqr :: Num a => a -> a.  it means "a" has to be a Num

--2. pass function as parameter
compose f g x = f (g x)
--compose :: (t1->t) -> (t2->t1) -> t2 -> t

--3. Lambda form
(\x -> x + 1) 41
--42