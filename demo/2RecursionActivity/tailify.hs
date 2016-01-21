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
  	