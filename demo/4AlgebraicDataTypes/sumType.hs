data Contest = Rock | Sci | Paper

data Mylist = Cons Int Mylist
            | Nil
 deriving Show

insertSorted a Nil = Cons a Nil
insertSorted a (Cons b bs)
     | a < b = Cons a (Cons b bs)
     | otherwise = Cons b $ insertSorted a bs

--insertSorted 3 (Cons 2 (Cons 4 Nil))

data ZList a = ZCons a (ZList a)
             | ZNil
           deriving Show


