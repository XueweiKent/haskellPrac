import Debug.Trace

--1. base case first
fact 0 = 1
fact n = n * fact (n-1)

--2. no base case
removeNegatives [] = []
removeNegatives (x:xs) | x < 0 = result
                       | otherwise = x : result
  where result = removeNegatives xs

--3. no efficient? 
reverse' [] = []
reverse' (x:xs) = trace (show xs) (reverse' xs) ++ [x]

reverse'' xx = aux xx [] 
  where aux [] a     = a
        aux (x:xs) a = aux xs (x:a)

--4. a. base case first. b. induction
decList [] = []
decList (x:xs) = (x-1) : (decList xs)