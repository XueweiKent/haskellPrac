inc x = x + 1

fact 0 = 1
fact n = n * fact (n-1)

len [] = 0
len (x:xs) = 1 + len xs

guess [] = []
guess (x:xs) = guess [y | y <- xs, y < x]
               ++ [x] ++
               guess [y | y <- xs, y >= x]

ismult n = (n `mod` 3) * (n `mod` 5) == 0


euler1 = foldl (+) 0 $ filter ismult [1..999]

nats = 1 : map (+1) nats

lprime x p =
   if x == p then x
      else if (x `mod` p) == 0 
           then lprime (x `div` p) p 
           else lprime x (p + 1)
                            

