--Exercício 1

pertence _ [] = False
pertence n (x:xs) = if  n==x then True else pertence n xs

--Exercício 2

intersecao _ [] = []
intersecao [] _ = []
intersecao (x:xs) (a:b) | pertence x (a:b) = x : intersecao xs (a:b)
                        | otherwise = intersecao xs (a:b)

--Exercício 3

inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--Exercício 4

nprimeiros 0 _ = []
nprimeiros n [] = []
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs

nultimos n [] = []
nultimos n (x:xs) = inverso (nprimeiros n (inverso (x:xs)))

