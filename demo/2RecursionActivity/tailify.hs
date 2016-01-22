import Debug.Trace

maxList [xs] = aux xs (head xs)
  where 
  	aux [] acc = acc
  	aux (x:xs) acc = aux xs (max x acc)


fib n = aux 1 1 n
  where 
    aux a b 1 = b
    aux a b 2 = a
    aux a b n = aux (a+b) a (n-1)


cutHead [] acc chg = acc
cutHead [x:xs] acc chg = cutHead [xs] (acc+(x+chg)*length[x:xs]) ((x+chg)*length[x:xs])

chop [x] = [x-1]
chop (0:xs) = 0 : chop xs
chop (x0:x1:xs) = x0 - 1 : x1 + 1 + length xs : xs

lchop heads = aux heads (length heads)
  where
  	aux [0] level = [0]
  	aux [x] level = [x-1]
  	aux (0:xs) level = 0 : aux xs (level - 1)
  	aux (x0:x1:xs) level = x0 - 1 : x1 + level - 1 : xs


maxL (x:xs) = aux xs x
  where 
  	aux [] acc = acc
  	aux (y:ys) acc = aux ys (max y acc)

myFib n = aux 1 1 n
  where 
  	aux a b 1 = a
  	aux a b n = aux b (a+b) (n-1)

bigChop heads = aux heads 0
  where 
  	aux [] carry = carry
  	aux (x:xs) carry = aux xs ((carry+x) * (length (x:xs)))

smallChop (x:xs) = aux x xs 
  where 
    elimZero (x:xs) = if(x==0) then xs else (x:xs)
    aux 0 (y:ys) = aux y ys
    aux x []     = if (x>0) then [x-1] else [0]
    aux x (y:ys) = elimZero ( (x-1):(y+length(ys)+1):ys )

countSmallChop xx = aux xx 0
  where 
    aux [0] n = n
    aux xx  n = aux (smallChop xx) n+1