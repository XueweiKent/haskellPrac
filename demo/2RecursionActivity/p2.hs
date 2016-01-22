import Debug.Trace

fact 0 = 1
fact n = n * fact (n-1)

removeNegatives [] = []
removeNegatives (x:xs)
     | x < 0     = result
     | otherwise = x : result
  where result = removeNegatives xs

rev xx = aux xx []
  where aux [] acc = acc
        aux (x:xs) acc =
          trace (show (x:xs) ++ " " ++ show acc)
                aux xs (x : acc)

decList (x:xs) = x - 1 : decList xs
decList [] = []

maxList [x] = x
maxList (x:xs) = max x (maxList xs)

tmaxList (x:xs) = aux xs x
  where
    aux []     acc = acc
    aux (x:xs) acc = aux xs (max x acc)

fib n = aux 1 0 n
  where aux a b 0 = a
        aux a b n = --trace (show a ++ " " ++ show b)
                    aux (a+b) a (n-1)

hydra = [9,0,0,0,0,0,0,0,0] :: [Int]
-- [8,8,0,0,0,0,0,0,0]
-- [8,7,7,0,0,0,0,0,0]
-- [7,16,0,0,0,0,0,0,0]


chop xx = aux xx (length xx)
  where aux [] _ = []
        aux (0:xs) lev = 0 : aux xs (lev - 1)
        aux (x:y:xs) lev = x-1:y + lev - 1:xs
        aux [x] _ = [x-1]

hercules [0,0,0,0,0,0,0,0,0] chops = chops
hercules xx chops = hercules (chop xx) (1 + chops)


  x : xx

  new Node(x,xx)
