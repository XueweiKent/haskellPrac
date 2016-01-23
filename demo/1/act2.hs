inc x = x + 1

fact 0 = 1
fact n = n * fact (n-1)

len [] = 0
len (x:xs) = 1 + len xs

x = [8,6,7,5,3,0,9]

guess [] = []
guess (x:xs) = guess [y | y <- xs, y < x]
               ++ [x] ++
               guess [y | y <- xs, y >= x]

-- Euler 1

ismult x = (x `mod` 3) * (x `mod` 5) == 0

euler1 = sum $ filter ismult [1..999]

nats = 1 : map inc nats

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


-- Euler 3

getLargestFactor n pf =
  if (n==pf) then n
  else if (n `mod` pf == 0)
          then getLargestFactor (n `div` pf) pf
       else getLargestFactor n (pf + 1)

glf n pf | n == pf         = n
         | n `mod` pf == 0 = glf (n `div` pf) pf
         | otherwise       = glf n (pf + 1)

euler3 = getLargestFactor 600851475143 2
