import Debug.Trace

--1. base case first
fact 0 = 1
fact n = n * fact (n-1)

--2. no base case
removeNegatives [] = []
removeNegatives (x:xs) | x < 0 = result
                       | otherwise = x : result
  where result = removeNegatives xs

--3. illegal name
reverse' [] = []
reverse' (x:xs) = trace (show xs) (reverse xs) ++ [x]

