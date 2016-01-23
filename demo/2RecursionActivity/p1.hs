import Debug.Trace

fact 0 = 1
fact n = n * fact (n-1)

removeNegatives [] = []
removeNegatives (x:xs) | x < 0     = result
                       | otherwise = x : result
               where result = removeNegatives xs

reverse xx = aux xx []
  where aux [] a     = a
        aux (x:xs) a = trace (show (x: xs) ++ " " ++ show a) aux xs (x:a)


declist (x:xs) = x - 1 : declist (x:xs)
declist [] = []

maxList xx = aux xx (head xx)
  where
      aux []     acc = acc
      aux (x:xs) acc = aux xs (max x acc)

fib n = aux 1 0 n
  where aux a b 1 = a
        aux a b n = aux (a+b) a (n-1)

chop heads = aux heads (length heads)
 where
  aux [0]   level = [0]
  aux [x]   level = [x-1]
  aux (0:xs) level = 0 : aux xs (level - 1)
  aux (x0:x1:xs) level = x0 - 1 : x1 + level - 1 : xs


--remneg (x:xs) =
--  if x < 0
     --then result
  --else if x > 0 then x + 1 : result
       --else x : result
  --where result = remneg xs

remneg [] = []
remneg (x:xs) | x < 0 = result
              | x > 0 = x+1 : result
              | True  = x   : result
  where result = remneg xs
