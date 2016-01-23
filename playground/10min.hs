module Main where

readSquare = do{
	n <- readLn;
	print (n^2)
}

rightOrWrong = do putStrLn "2+2?" 
                  x <- readLn
                  if x == 4
                      then putStrLn "right"
                      else putStrLn "wrong"

listPlay = do{
	let n=[1 .. 5];
	map (+ 2) n;
	filter (> 2) n
}

pairPlay = do{
	let p = (1, 2);
	fst p;
	snd p;
	map fst [(1, 2), (3, 4), (5, 6)]
}

classify age = case age of 0 -> "newborn"
                           1 -> "infant"
                           2 -> "toddler"
                           _ -> "senior citizen"

main = do putStrLn "main start"
          putStrLn "hoho"
